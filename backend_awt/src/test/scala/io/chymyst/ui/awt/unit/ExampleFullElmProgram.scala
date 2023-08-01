package io.chymyst.ui.awt.unit

import io.chymyst.ui.elm.Elm.{Consume, ConsumeOrCancel, Program}
import io.chymyst.ui.elm.View

import scala.concurrent.duration.{DurationInt, FiniteDuration}

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

  // Command consists of a random answer (success or fail) after a delay of 0.5 seconds.
  val runCommand: C[E] => Consume[E] = {
    case () =>
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.Future
      val result: Consume[E] = { consume =>
        Future {
          Thread.sleep(500L)
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
                          isListening1: Boolean = false,
                          isListening2: Boolean = false,
                        )

  type M = Model // Count clicks and indicate whether control buttons are shown.

  sealed trait Events

  case object Increment extends Events

  case object Reset extends Events

  case object ToggleButtons extends Events

  final case class TimerTick(interval: FiniteDuration) extends Events

  case object StartTimer1 extends Events

  case object StartTimer2 extends Events

  case object StopAllTimers extends Events

  case object SendCommand extends Events

  case object CommandSucceeded extends Events

  case object CommandFailed extends Events

  type E = Events

  val displayView: M => View[E] = { m =>
    val buttons = View.TileH(View.TileH(
      View.Button("Increment", Increment), View.Button("Reset", Reset),
    ), View.TileH(
      View.Button("Start timer1", StartTimer1), View.Button("Start timer2", StartTimer2),
    )
    )
    val clicksDisplay = View.TileH(View.TileH(
      View.Label(s"${m.clicks} clicks"), View.Label(m.lastCommandStatus match {
        case Some(value) => s"Last command $value"
        case None => "No last command"
      })), View.TileH(View.Label(m.lastTimerTickHadInterval match {
      case Some(value) => s"Last ticker had interval $value"
      case None => "No last ticker event"
    }), View.Button(if (m.showButtons) "Hide buttons" else "Show buttons", ToggleButtons)
    ))
    val buttons2 = View.TileH(View.Button("Send command", SendCommand), View.Button("Stop all timers", StopAllTimers))
    View.TileV(
      if (m.showButtons) View.TileV(
        clicksDisplay,
        buttons,
      ) else clicksDisplay,

      buttons2,
    )
  }

  val updateModel: PartialFunction[E, M => M] = {
    case Increment => m => m.copy(clicks = m.clicks + 1)
    case Reset => _.copy(clicks = 0, lastTimerTickHadInterval = None, lastCommandStatus = None)
    case ToggleButtons => m => m.copy(showButtons = !m.showButtons)
    case StartTimer1 => _.copy(isListening1 = true)
    case StartTimer2 => _.copy(isListening2 = true)
    case StopAllTimers => _.copy(isListening1 = false, isListening2 = false)
    case TimerTick(duration) => _.copy(lastTimerTickHadInterval = Some(duration))
    case CommandFailed => _.copy(lastCommandStatus = Some(false))
    case CommandSucceeded => _.copy(lastCommandStatus = Some(true))
  }

  sealed trait Subscriptions[+E]

  final case class TimerSub[E](interval: FiniteDuration) extends Subscriptions[E]

  val interval1: FiniteDuration = 1200.millis
  val interval2: FiniteDuration = 2.seconds

  type S[E] = Subscriptions[E]
  type C[E] = Unit

  val program: Program[M, View, E, C, S] = new Program[M, View, E, C, S] {
    override def init: M = Model()

    override def display: M => View[E] = displayView

    override def update: PartialFunction[E, M => M] = updateModel

    override def commands: PartialFunction[E, M => Seq[C[E]]] = {
      case SendCommand => _ => Seq(())
    }

    override def subscriptions: M => Set[S[E]] = m => (if (m.isListening1) Set(TimerSub(interval1)) else Set[S[E]]()) ++ (if (m.isListening2) Set(TimerSub(interval2)) else Set())
  }
}
