package io.chymyst.ui.awt.unit

import io.chymyst.ui.elm.Elm.SimpleProgram
import io.chymyst.ui.elm.View

object ExampleSimpleElmProgram {
  final case class Model(clicks: Int = 0, showButtons: Boolean = true)

  type M = Model // Count clicks and indicate whether control buttons are shown.

  sealed trait ExampleEvents

  case object Increment extends ExampleEvents

  case object Reset extends ExampleEvents

  case object ToggleButtons extends ExampleEvents

  type E = ExampleEvents // Three buttons. "Increment", "Reset", "Show/hide other buttons".

  val display: M => View[E] = { m =>
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

  val update: M => E => M = m => {
    case Increment => m.copy(clicks = m.clicks + 1)
    case Reset => m.copy(clicks = 0)
    case ToggleButtons => m.copy(showButtons = !m.showButtons)
  }

  val program: SimpleProgram[M, View, E] = (Model(), display, update)
}
