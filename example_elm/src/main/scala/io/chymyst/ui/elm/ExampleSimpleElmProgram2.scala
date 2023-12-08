package io.chymyst.ui.elm

import io.chymyst.ui.elm.Elm.SimpleProgram

object ExampleSimpleElmProgram2 {
  final case class Model(clicks1: Int, clicks2: Int, currentIs1or2: Int, showButtons: Boolean)

  type M = Model // Count clicks and indicate whether control buttons are shown.

  val initModel: M = Model(clicks1 = 0, clicks2 = 0, currentIs1or2 = 0, showButtons = true)

  sealed trait ExampleEvents

  case class SetCurrent(index: Int) extends ExampleEvents

  case object Increment extends ExampleEvents

  case object Reset extends ExampleEvents

  case object ToggleButtons extends ExampleEvents

  type E = ExampleEvents

  val display: M => View[E] = { m =>
    val buttons = View.TileLeftToRight(
      View.Choice(items = Seq("clicks 1", "clicks2  "), onSelect = SetCurrent, selectedIndex = m.currentIs1or2),
      View.Button("Increment", Increment),
      View.Button("Reset", Reset),
    )
    val clicksDisplay = View.TileLeftToRight(
      View.Label(s"${m.clicks1} clicks1"),
      View.Label(s"${m.clicks2} clicks2"),
      View.Button(if (m.showButtons) "Hide buttons" else "Show buttons", ToggleButtons),
    )
    if (m.showButtons) View.TileTopToBottom(
      clicksDisplay,
      buttons,
    ) else clicksDisplay
  }

  val update: M => E => M = m => {
    case SetCurrent(i) => m.copy(currentIs1or2 = i)
    case Increment => if (m.currentIs1or2 == 0) m.copy(clicks1 = m.clicks1 + 1) else m.copy(clicks2 = m.clicks2 + 1)
    case Reset => if (m.currentIs1or2 == 0) m.copy(clicks1 = 0) else m.copy(clicks2 = 0)
    case ToggleButtons => m.copy(showButtons = !m.showButtons)
  }

  val program: SimpleProgram[M, View, E] = (initModel, display, update)
}
