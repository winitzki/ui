package io.chymyst.ui.elm

import io.chymyst.ui.elm.View.Label.LabelAlignment
import io.chymyst.ui.elm.View.LayoutPreferences

trait View[+E] { // Covariance makes it easier to use subtypes of Event in views.
  def layoutPreferences = LayoutPreferences()
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

  final case class LayoutPreferences(
                                      fillsHorizontalSpace: Boolean = false,
                                      fillsVerticalSpace: Boolean = false,
                                      typicallyWide: Boolean = true,
                                      typicallyTall: Boolean = false,
                                    )

  final case class Button[+E](text: String, onClick: E) extends View[E]

  final case class Choice[+E](items: Seq[String], onClick: Int => E, selectedIndex: Int) extends View[E]

  final case class CheckBox[+E](text: String, onClick: Boolean => E, state: Boolean) extends View[E]

  final case class TextArea[+E](textLines: Seq[String], wrap: Boolean) extends View[E] {
    override def layoutPreferences = LayoutPreferences(fillsHorizontalSpace = true, fillsVerticalSpace = true, typicallyWide = true, typicallyTall = true)
  }

  final case class TextInputField[+E](currentText: String, onClick: String => E) extends View[E] {
    override def layoutPreferences = LayoutPreferences(fillsHorizontalSpace = true)
  }

  //Define simple layout combinators (tile vertical, tile horizontal) building a larger `View` out of smaller `View`s.

  final case class TileLeftToRight[+E](views: View[E]*) extends View[E]

  final case class TileTopToBottom[+E](views: View[E]*) extends View[E]
}
