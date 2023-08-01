package io.chymyst.ui.elm

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit, ConcurrentMap, ConcurrentHashMap}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.util.{Failure, Success}

object Elm {

  trait Program[M, V[_], E, C[_], S[_]] {

    def init: M

    // Optimization: whether a given event is "enabled". If disabled, it means the event will be ignored.
    def enable: E => M => Boolean = _ => _ => true

    // Enable or disable listening to recurrent "external events" such as timers or continuous inputs. Actions necessary to start or stop those events will be executed automatically.
    def subscriptions: M => Set[S[E]] = _ => Set()

    // Produce a view, given a model.
    def display: M => V[E]

    // Produce a new model value, given an event and a current model value.
    def update: PartialFunction[E, M => M] = _ => identity

    // Run "external commands", given an event and a current model value. An external command of type C[E] will typically execute an action that may generate zero or more events.
    def commands: PartialFunction[E, M => Seq[C[E]]] = _ => _ => Seq()
  }

  type SimpleProgram[M, V[_], E] = (M, M => V[E], M => E => M)

  // We never stop the runloop. This code is independent of any backends and can be run on any thread.
  def run[M, V[_], E](program: SimpleProgram[M, V, E], render: V[E] => Future[E])(implicit ec: ExecutionContext): Unit = {
    val (m, display, update) = program
    val view = display(m)
    val event: Future[E] = render(view)
    event.onComplete {
      case Success(e) =>
        val newM = update(m)(e)
        run((newM, display, update), render)
      case Failure(error) =>
        println(s"DEBUG: this should not happen, $event failed: $error")
    }
  }

  type Cancel = Unit => Unit // When this function is called, events should no longer get consumed.
  type ConsumeOrCancel[E] = (E => Unit) => Cancel // Call the function of type E => Unit to consume an event. Call the function of type Cancel to cancel the subscription. No further events should be consumed after the Cancel function returns.
  type Consume[E] = (E => Unit) => Unit // Call the function of type E => Unit to consume an event.

  // This creates a runloop that can be started and stopped.
  final class RunLoop[M, V[_], E, C[_], S[_]](
                                               program: Program[M, V, E, C, S],
                                               render: V[E] => Future[E], // Render the view graphically and return the first user-generated event asynchronously.
                                               runCommand: C[E] => Consume[E], // Run the command and consume its resulting events.
                                               listen: S[E] => ConsumeOrCancel[E], // Allow these events to enter the runloop.
                                             ) {
    // Single-thread executor for event queue.
    private val eventExecutor: ThreadPoolExecutor = new ThreadPoolExecutor(1, 1, 1, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]())
    private val singleThreadedExecutor: ExecutionContext = ExecutionContext.fromExecutor(eventExecutor)

    @volatile private var currentModel: M = program.init

    final case class RunnableForSubscriptions(sub: S[E], e: E) extends Runnable {
      override def run(): Unit = inputStep(e)
    }

    private val consume: E => Unit = e => eventExecutor.execute(() => inputStep(e))

    private val currentSubs: ConcurrentMap[S[E], Cancel] = new ConcurrentHashMap[S[E], Cancel]()

    private def outputStep(): Unit = {
      val oldM = currentModel
      val view = program.display(oldM)
      val subs = program.subscriptions(oldM)
      val eventFromUI: Future[E] = render(view)
      eventFromUI.onComplete {
        case Success(e) => consume(e)
        case Failure(error) => println(s"DEBUG: this should not happen, $eventFromUI failed: $error")
      }(singleThreadedExecutor)
      // Find out which subscriptions are new and which have disappeared.
      val oldSubs = Set.from(currentSubs.keySet.asScala)
      val addedSubs = subs.diff(oldSubs)
      val deletedSubs = oldSubs.diff(subs)
      addedSubs foreach { sub =>
        val cancel = listen(sub)(e => eventExecutor.execute(RunnableForSubscriptions(sub, e)))
        currentSubs.put(sub, cancel)
      }
      deletedSubs foreach { sub =>
        val cancel = currentSubs.get(sub)
        cancel(())
        // Remove all tasks that have already been scheduled for this subscription.
        eventExecutor.getQueue.removeIf((t: Runnable) => t.isInstanceOf[RunnableForSubscriptions] && t.asInstanceOf[RunnableForSubscriptions].sub == sub)
      }
    }

    private def inputStep(event: E): Unit = {
      val oldM = currentModel
      val enabled = program.enable(event)(oldM)
      lazy val modelUpdateNeeded = program.update.isDefinedAt(event)
      if (enabled) {
        if (modelUpdateNeeded) {
          currentModel = program.update(event)(oldM)
        }
        if (program.commands.isDefinedAt(event)) {
          val commands = program.commands(event)(oldM)
          commands foreach { command =>
            runCommand(command)(consume)
          }
        }
        if (enabled && modelUpdateNeeded) outputStep()
      }
    }

    def start(): Unit = outputStep()

    def stop(): Unit = {
      eventExecutor.shutdownNow()
    }
  }

}
