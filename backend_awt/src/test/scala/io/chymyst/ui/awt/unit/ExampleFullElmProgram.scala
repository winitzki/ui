package io.chymyst.ui.awt.unit

import io.chymyst.ui.elm.Elm.{Consume, ConsumeOrCancel, Program}
import io.chymyst.ui.elm.View

import scala.concurrent.duration.FiniteDuration

object ExampleFullElmProgram {
  val listen: S[E] => ConsumeOrCancel[E] = {
    case TimerSub(duration) =>
      import java.util.{Timer, TimerTask}
      val timer = new Timer()
      consume =>
        val timerTask = new TimerTask {
          override def run(): Unit = consume(TimerTick(duration))
        }
        timer.scheduleAtFixedRate(timerTask, 0L, duration.toMillis)
        _ => timer.cancel()
  }

  // Command consists of a random answer (success or fail) after a delay of 2 seconds.
  val runCommand: C[E] => Consume[E] = {
    case () =>
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future
      val result: Consume[E] = { consume =>
        Future {
          Thread.sleep(2000L)
          consume(if (scala.util.Random.nextBoolean()) CommandSucceeded else CommandFailed)
        }
      }
      result
  }

  final case class Model(
                          clicks: Int = 0,
                          showButtons: Boolean = true,
                          lastCommandStatus: Option[Boolean] = None,
                          lastTimerTickHadInterval: Option[FiniteDuration] = None,
                        )

  type M = Model // Count clicks and indicate whether control buttons are shown.

  sealed trait ExampleEvents

  case object Increment extends ExampleEvents

  case object Reset extends ExampleEvents

  case object ToggleButtons extends ExampleEvents

  final case class TimerTick(interval: FiniteDuration) extends ExampleEvents

  final case class StartTimer(interval: FiniteDuration) extends ExampleEvents

  case object StopAllTimers extends ExampleEvents

  case object SendCommand extends ExampleEvents

  case object CommandSucceeded extends ExampleEvents

  case object CommandFailed extends ExampleEvents

  type E = ExampleEvents // Three buttons. "Increment", "Reset", "Show/hide other buttons".

  val displayView: M => View[E] = { m =>
    val buttons = View.TileH(
      View.Button("Increment", Increment), View.Button("Reset", Reset)
    )
    val clicksDisplay = View.TileH(
      View.Label(s"${m.clicks} clicks"), View.Button(if (m.showButtons) "Hide buttons" else "Show buttons", ToggleButtons)
    )
    if (m.showButtons) View.TileV(
      clicksDisplay,
      buttons,
    ) else clicksDisplay
  }

  val updateModel: M => E => M = m => {
    case Increment => m.copy(clicks = m.clicks + 1)
    case Reset => m.copy(clicks = 0)
    case ToggleButtons => m.copy(showButtons = !m.showButtons)
  }

  sealed trait Subscriptions[E]

  final case class TimerSub[E](interval: FiniteDuration) extends Subscriptions[E]

  type S[E] = Subscriptions[E]
  type C[E] = Unit

  val program: Program[M, View, E, C, S] = new Program[M, View, E, C, S] {
    override def init: M = Model()

    override def display: M => View[E] = displayView

    override def update: PartialFunction[E, M => M] = e => m => updateModel(m)(e)

    override def commands: PartialFunction[E, M => Seq[C[E]]] = {
      case SendCommand => _ => Seq(())
    }

    override def subscriptions: M => Set[S[E]] = super.subscriptions
  }
}
