package io.chymyst.ui.awt

import io.chymyst.ui.elm.{LabelAlignment, View}

import java.awt._
import java.awt.event.{ActionEvent, WindowAdapter, WindowEvent}
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.{Box, BoxLayout}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Success

object AwtRunner {
  lazy val (frame, panel) = getFrameAndPanel()

  def toAwtLabelAlignment: LabelAlignment => Int = {
    case LabelAlignment.Left => Label.LEFT
    case LabelAlignment.Center => Label.CENTER
    case LabelAlignment.Right => Label.RIGHT
  }

  def getFrameAndPanel() = {
    val f = new Frame()
    f.setTitle("AWT Runner example")
    f.setBounds(400, 400, 640, 480)
    f.setLayout(new BoxLayout(f, BoxLayout.X_AXIS))  // TODO better layout, or just use Swing instead of AWT.
    val p = new Panel()
    p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS))
    p.setName("main panel")
    f.add(p)
    f.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent): Unit = {
        f.dispose()
        System.exit(0)
      }
    })
    //    p.setVisible(true) // Not necessary.
    //    f.setVisible(true) // Must be done later after some UI components are added to the panel.
    (f, p)
  }

  val counter = new AtomicInteger(0)

  // Return the first future that succeeds.
  def first[T](f: Future[T], g: Future[T]): Future[T] = {
    val p = Promise[T]
    f.foreach { x => p.trySuccess(x) }
    g.foreach { x => p.trySuccess(x) }
    p.future
  }

  def renderView[E](view: View[E]): Future[E] = {
    def render(subview: View[E], inPanel: => Panel = panel, clearPanel: Boolean = true): Future[E] = {
      //    println(s"DEBUG: adding view $view in panel $inPanel; panel is valid: ${inPanel.isValid}, on dispatch thread: ${EventQueue.isDispatchThread}")
      val p = Promise[E]
      //    EventQueue.invokeLater { () =>
      if (clearPanel) inPanel.removeAll() //else println(s"DEBUG: Not clearing panel for view $view")
      subview match {
        case View.Label(text, alignment) =>
          inPanel.add(new Label(text, toAwtLabelAlignment(alignment)))

        case View.Button(text, event: E) =>
          val button = new Button(text)
          button.addActionListener { (_: ActionEvent) =>
            p.trySuccess(event)
          }
          inPanel.add(button)

        case View.TileH(left, right) =>
          val n = new Panel()
          n.setLayout(new BoxLayout(n, BoxLayout.X_AXIS)) // We are using a Swing layout manager here, and it seems to be working.
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          val f1 = render(left, n, false)
          val f2 = render(right, n, false)
          n.add(Box.createHorizontalGlue)
          first(f1, f2).onComplete { case Success(e: E) => p.success(e) }

        case View.TileV(top, bottom) =>
          val n = new Panel()
          n.setLayout(new BoxLayout(n, BoxLayout.Y_AXIS))
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          val f1 = render(top, n, false)
          val f2 = render(bottom, n, false)
          n.add(Box.createVerticalGlue)
          first(f1, f2).onComplete { case Success(e: E) => p.success(e) }
      }
      frame.setVisible(true) // Need to do this after any changes in layout or adding components. Otherwise nothing is shown and frame.isValid == false.
      //    }
      p.future
    }

    render(view)
  }
}
