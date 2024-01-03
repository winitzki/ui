package io.chymyst.ui.elm

import io.chymyst.ui.elm.Elm.SimpleProgram

object ExampleTextArea {
  final case class Model(lines: Seq[String], wrap: Boolean)

  type M = Model

  val initModel: M = Model(Seq("first line", "second line", "long line: " + "." * 1000), wrap = false)

  sealed trait ExampleEvents

  case class SetWrap(wrap: Boolean) extends ExampleEvents

  type E = ExampleEvents

  val display: M => View[E] = { m =>
    View.TileTopToBottom(
      View.TextArea(m.lines, wrap = m.wrap),
      View.CheckBox("Word wrap", SetWrap, m.wrap),
    )
  }

  val update: M => E => M = m => {
    case SetWrap(wrap) => m.copy(wrap = wrap)
  }

  val program: SimpleProgram[M, View, E] = (initModel, display, update)
}

object ExampleTextInput {
  val program: SimpleProgram[String, View, Boolean] = ("initial string", s => View.TileLeftToRight(
    View.Button("reset", true),
    View.Button("do nothing", false),
    View.TextInputField(s, t => println(s"current text = '$t'") != ()),
  ), s => e => if (e) "" else s)
}
