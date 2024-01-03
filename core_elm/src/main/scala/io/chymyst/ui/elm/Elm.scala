package io.chymyst.ui.elm

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import scala.jdk.CollectionConverters.SetHasAsScala

object Elm {

  trait Program[M, V[_], E, C[_], S[_]] {

    def init: M

    // Optimization: whether a given event requires an updated view. If `false`, it means the event may update the model but it is not necessary to update the view.
    def needViewRefresh: PartialFunction[E, M => Boolean] = _ => _ => true

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
  def runSimpleProgram[M, V[_], E](program: SimpleProgram[M, V, E], render: V[E] => Consume[E]): Unit = {
    val (m, display, update) = program
    val view = display(m)
    render(view) { e: E => // We assume that this callback will be always called on the GUI thread.
      val newM = update(m)(e)
      runSimpleProgram((newM, display, update), render)
    }
  }

  type Cancel = Unit => Unit // When this function is called, events should no longer get consumed.
  type ConsumeOrCancel[E] = (E => Unit) => Cancel // Call the function of type E => Unit to consume an event. Call the function of type Cancel to cancel the subscription. No further events should be consumed after the Cancel function returns.
  type Consume[E] = (E => Unit) => Unit // Call the function of type E => Unit to consume an event.

  trait UiBackend[V[_], E] {
    // Render the view graphically and return the first user-generated event asynchronously by calling the callback on the GUI thread.
    def renderView: V[E] => Consume[E]

    // Call the callback on the GUI thread, even when this function was called on another thread.
    def runOnEventThread[L](label: L, callback: E => Unit): E => Unit = callback

    // Remove pending subscribed-to events created before a subscription is canceled. The events are identified by a value `label` in some way.
    def removePendingEvents[L](label: L): Unit = ()
  }

  trait EffectRunner[E, C[_], S[_]] {
    // Run the command and consume its resulting event(s). The backend will make sure that events go to the GUI thread. The command cannot be canceled once started.
    def runCommand: C[E] => Consume[E]

    // Consume subscribed-to events unless canceled. The backend will make sure that events go to the GUI thread. Events created before canceling but not yet delivered will not be delivered.
    def listen: S[E] => ConsumeOrCancel[E]
  }

  // This creates a runloop that can run a full Elm program given a backend and an effect runner.
  final class RunLoop[M, V[_], E, C[_], S[_]](
                                               program: Program[M, V, E, C, S],
                                             )(
                                               ui: UiBackend[V, E],
                                               effects: EffectRunner[E, C, S],
                                             ) {
    @volatile private var currentModel: M = program.init

    // Store the cancel function for each subscription.
    private val currentSubs: ConcurrentMap[S[E], Cancel] = new ConcurrentHashMap[S[E], Cancel]()

    private def outputStep(): Unit = {
      val oldM = currentModel
      val view = program.display(oldM)
      val subs = program.subscriptions(oldM)
      ui.renderView(view)(ui.runOnEventThread((), runSingleStep))
      // Find out which subscriptions are new and which have disappeared.
      val oldSubs = Set.from(currentSubs.keySet.asScala)
      val addedSubs = subs.diff(oldSubs)
      val deletedSubs = oldSubs.diff(subs)
      addedSubs foreach { sub =>
        val cancel = effects.listen(sub)(ui.runOnEventThread(sub, runSingleStep))
        currentSubs.put(sub, cancel)
      }
      deletedSubs foreach { sub =>
        val cancel = currentSubs.remove(sub)
        ui.removePendingEvents(sub)
        cancel(())
      }
    }

    private def runSingleStep(event: E): Unit = {
      val oldM = currentModel
      val modelUpdateNeeded = program.update.isDefinedAt(event)
      if (modelUpdateNeeded) {
        currentModel = program.update(event)(oldM)
      }
      if (program.commands.isDefinedAt(event)) {
        val commands = program.commands(event)(oldM)
        commands foreach { command =>
          effects.runCommand(command)(ui.runOnEventThread((), runSingleStep))
        }
      }
      val needToUpdateView = program.needViewRefresh.isDefinedAt(event) && program.needViewRefresh(event)(oldM)
      if (needToUpdateView && modelUpdateNeeded) outputStep()
    }

    def start(): Unit = outputStep()
  }

}
