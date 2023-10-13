package io.chymyst.ui.swing

import io.chymyst.ui.elm.Elm.{Consume, UiBackend}
import io.chymyst.ui.elm.{LabelAlignment, View}

import java.awt.Container
import java.awt.event.ActionEvent
import javax.swing.{Box, BoxLayout, JButton, JFrame, JLabel, JPanel, SwingConstants, WindowConstants}

final class SwingRunner {
  private def getFrameAndContentPane(): (JFrame, Container) = {
    val f = new JFrame("Swing Runner")
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setBounds(400, 400, 640, 480)
    val p = f.getContentPane
    (f, p)
  }

  private lazy val (frame, panel) = getFrameAndContentPane()

  private def toSwingLabelAlignment: LabelAlignment => Int = {
    case LabelAlignment.Left => SwingConstants.LEADING
    case LabelAlignment.Center => SwingConstants.CENTER
    case LabelAlignment.Right => SwingConstants.TRAILING
  }

  def renderView[E](view: View[E]): Consume[E] = {
    def render(subview: View[E], inPanel: => Container = panel, clearPanel: Boolean = true): Consume[E] = consume => {
      //    println(s"DEBUG: adding view $view in panel $inPanel; panel is valid: ${inPanel.isValid}, on dispatch thread: ${EventQueue.isDispatchThread}")
      //    EventQueue.invokeLater { () =>
      if (clearPanel) inPanel.removeAll() //else println(s"DEBUG: Not clearing panel for view $view")
      subview match {
        case View.Label(text, alignment) =>
          inPanel.add(new JLabel(text, toSwingLabelAlignment(alignment)))

        case View.Button(text, event: E) =>
          val button = new JButton(text)
          button.addActionListener { (_: ActionEvent) =>
            consume(event)
          }
          inPanel.add(button)

        case View.TileH(left, right) =>
          val n = new JPanel()
          n.setLayout(new BoxLayout(n, BoxLayout.X_AXIS)) // We are using a Swing layout manager here, and it seems to be working.
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          render(left, n, false)(consume)
          render(right, n, false)(consume)
          n.add(Box.createHorizontalGlue)

        case View.TileV(top, bottom) =>
          val n = new JPanel()
          n.setLayout(new BoxLayout(n, BoxLayout.Y_AXIS))
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          render(top, n, false)(consume)
          render(bottom, n, false)(consume)
          n.add(Box.createVerticalGlue)
      }
      frame.setVisible(true) // Need to do this after any changes in layout or adding components. Otherwise nothing is shown and frame.isValid == false.
    }

    render(view)
  }
}
