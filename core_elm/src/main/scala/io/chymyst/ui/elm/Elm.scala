package io.chymyst.ui.elm

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Elm {

  trait Program[M, V[_], E, C[_], S[_]] {

    // Optimization: whether a given event is "enabled". If disabled, it means the event will be ignored.
    def enable: E => M => Boolean = _ => _ => true

    // Enable or disable listening to recurrent "external events" such as timers or continuous inputs. Actions necessary to start or stop those events will be executed automatically.
    def subscriptions: M => Seq[S[E]] = _ => Seq()

    // Produce a view, given a model.
    def display: M => V[E]

    // Produce a new model value, given an event and a current model value.
    def update: E => M => M = _ => identity

    // Run "external commands", given an event and a current model value. An external command of type C[E] will typically execute an action that may generate zero or more events.
    def commands: E => M => Seq[C[E]] = _ => _ => Seq()
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

  def run[M, V[_], E, C[_], S[_]](
                                   program: Program[M, V, E, C, S],
                                   render: V[E] => Future[E],
                                   runCommands: Seq[C[E]] => Future[E],
                                   listen: Seq[S[E]] => Future[E],
                                 ): Unit = {
//    val (m, display, update) = program
//    val view = display(m)
//    val event: Future[E] = render(view)
//    event.onComplete {
//      case Success(e) =>
//        val newM = update(m)(e)
//        run((newM, display, update), render)
//      case Failure(error) =>
//        println(s"DEBUG: this should not happen, $event failed: $error")
//    }
  }
}