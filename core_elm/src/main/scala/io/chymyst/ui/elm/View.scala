package io.chymyst.ui.elm

import io.chymyst.ui.elm.View.Label.LabelAlignment

trait View[+E] { // Covariance makes it easier to use subtypes of Event in views.
  /** Does this view typically look like a box that's horizontally long, vertically short?
   * Example: a one-line text label.
   *
   * @return `true` if so
   */
  def isHorizontal: Boolean = true

  /** Does this view typically look like a box that's horitontally short, vertically long?
   * Example: a vertical slider.
   *
   * @return `true` if so
   */
  def isVertical: Boolean = false
}

object View {
  // Built-in components.
  final case class Label[+E](text: String, align: LabelAlignment = LabelAlignment.Center) extends View[E]

  object Label {
    sealed trait LabelAlignment

    object LabelAlignment {
      final case object Left extends LabelAlignment

      final case object Center extends LabelAlignment

      final case object Right extends LabelAlignment
    }
  }

  final case class Button[+E](text: String, onClick: E) extends View[E]

  final case class Choice[+E](items: Seq[String], onClick: Int => E, selectedIndex: Int) extends View[E]

  final case class CheckBox[+E](text: String, onClick: Boolean => E, state: Boolean) extends View[E]

  final case class TextArea[+E](textLines: Seq[String], wrap: Boolean) extends View[E] {
    override def isHorizontal: Boolean = false

    override def isVertical: Boolean = false
  }

  //Define simple layout combinators (tile vertical, tile horizontal) building a larger `View` out of smaller `View`s.

  final case class TileLeftToRight[+E](views: View[E]*) extends View[E]

  final case class TileTopToBottom[+E](views: View[E]*) extends View[E]
}
