package io.chymyst.ui.awt

import io.chymyst.ui.elm.{LabelAlignment, View}

import java.awt._
import java.awt.event.{ActionEvent, WindowAdapter, WindowEvent}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Success

object AwtRunner {
  lazy val (frame, panel) = getFrameAndPanel()

  def labelAlignment: LabelAlignment => Int = {
    case LabelAlignment.Left => Label.LEFT
    case LabelAlignment.Center => Label.CENTER
    case LabelAlignment.Right => Label.RIGHT
  }

  def getFrameAndPanel() = {
    val f = new Frame()
    f.setTitle("AWT Runner example")
    f.setSize(400, 400)
    f.setLayout(new GridLayout(1, 1, 0, 0))
    val p = new Panel(new GridLayout(1, 1, 5, 5))
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

  def render[V[_], E](view: V[E], inPanel: => Panel = panel, clear: Boolean = true): Future[E] = {
    //    println(s"DEBUG: adding view $view in panel $inPanel; panel is valid: ${inPanel.isValid}, on dispatch thread: ${EventQueue.isDispatchThread}")
    val p = Promise[E]
    //    EventQueue.invokeLater { () =>
    if (clear) inPanel.removeAll() //else println(s"DEBUG: Not clearing panel for view $view")
    view match {
      case View.Label(text, alignment) =>
        inPanel.add(new Label(text, labelAlignment(alignment)))

      case View.Button(text, event: E) =>
        val button = new Button(text)
        button.addActionListener { (e: ActionEvent) =>
          p.trySuccess(event)
        }
        inPanel.add(button)

      case View.TileH(left, right) =>
        val n = new Panel(new GridLayout(1, 2, 5, 5))
        n.setName(s"Panel for $view")
        //        n.setVisible(true) // Not necessary.
        inPanel.add(n)
        first(
          render(left, n, false),
          render(right, n, false),
        ).onComplete { case Success(e: E) => p.success(e) }

      case View.TileV(top, bottom) =>
        val n = new Panel(new GridLayout(2, 1, 5, 5))
        n.setName(s"Panel for $view")
        //        n.setVisible(true) // Not necessary.
        inPanel.add(n)
        first(
          render(top, n, false),
          render(bottom, n, false),
        ).onComplete { case Success(e: E) => p.success(e) }
    }
    frame.setVisible(true) // Need to do this after any changes in layout or adding components. Otherwise nothing is shown and frame.isValid == false.
    //    }
    p.future
  }
}
