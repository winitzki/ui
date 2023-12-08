package io.chymyst.ui.elm

sealed trait View[+E] // Covariance makes it easier to use subtypes of Event in views.

sealed trait LabelAlignment

object LabelAlignment {
  final case object Left extends LabelAlignment

  final case object Center extends LabelAlignment

  final case object Right extends LabelAlignment
}

object View {
  // Basic components.

  final case class Label[+E](text: String, align: LabelAlignment = LabelAlignment.Center) extends View[E]

  final case class Button[+E](text: String, onClick: E) extends View[E]

  final case class Choice[+E](items: Seq[String], onSelect: Int => E, selectedIndex: Int) extends View[E]

  final case class CheckBox[+E](text: String, onClick: Boolean => E, state: Boolean) extends View[E]

  //Define simple layout combinators (tile vertical, tile horizontal) building a larger `View` out of smaller `View`s.

  final case class TileLeftToRight[+E](views: View[E]*) extends View[E]

  final case class TileTopToBottom[+E](views: View[E]*) extends View[E]
}
