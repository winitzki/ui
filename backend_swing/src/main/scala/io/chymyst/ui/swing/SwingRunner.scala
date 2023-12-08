package io.chymyst.ui.swing

import com.formdev.flatlaf.FlatLightLaf
import io.chymyst.ui.elm.Elm.Consume
import io.chymyst.ui.elm.View
import io.chymyst.ui.elm.View.Label.LabelAlignment

import java.awt.{Container, FlowLayout, GridLayout}
import java.awt.event.{ActionEvent, ItemEvent}
import javax.swing._

final class SwingRunner {
  // This sets up the initial window for the entire GUI. Add any global setup here.
  private def getFrameAndContentPane(): (JFrame, Container) = {
    // Setup flatlaf.
    FlatLightLaf.setup()

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
        case View.Choice(items, onSelect, selectedIndex) =>
          val n = new JComboBox(items.toArray)
          n.setSelectedIndex(selectedIndex)
          n.addItemListener((e: ItemEvent) => if (e.getStateChange == ItemEvent.SELECTED) consume(onSelect(n.getSelectedIndex)))
          inPanel.add(n)

        case View.CheckBox(text, onClick, state) =>
          val n = new JCheckBox(text, state)
          n.addItemListener((e: ItemEvent) => consume(onClick(e.getStateChange == ItemEvent.SELECTED)))
          inPanel.add(n)

        case View.TextArea(lines, wrap) =>
          val n = new JTextArea
          n.setRows(lines.length)
          n.setLineWrap(wrap)
          n.setText(lines.mkString("\n"))
          n.setEnabled(false) // Not editable.
          val s = new JScrollPane(n)
          s.setWheelScrollingEnabled(true)
          inPanel.add(s)

        case View.Label(text, alignment) =>
          inPanel.add(new JLabel(text, toSwingLabelAlignment(alignment)))

        case View.Button(text, event: E) =>
          val button = new JButton(text)
          button.addActionListener { (_: ActionEvent) =>
            consume(event)
          }
          inPanel.add(button)

        case View.TileLeftToRight(items@_*) =>
          val n = new JPanel()
          n.setLayout(new FlowLayout)
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          items.foreach { i => render(i, n, false)(consume) }
          n.add(Box.createHorizontalGlue)

        case View.TileTopToBottom(items@_*) =>
          val n = new JPanel()
          //          n.setLayout(new GridLayout(items.length, 1))
          n.setLayout(new BoxLayout(n, BoxLayout.Y_AXIS))
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          items.foreach { i => render(i, n, false)(consume) }

          n.add(Box.createVerticalGlue)
      }
      frame.setVisible(true) // Need to do this after any changes in layout or adding components. Otherwise nothing is shown and frame.isValid == false.
    }

    render(view)
  }
}
