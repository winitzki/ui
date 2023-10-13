package io.chymyst.ui.awt

import io.chymyst.ui.awt.AwtRunner.LabeledRunnable
import io.chymyst.ui.elm.Elm.{Consume, UiBackend}
import io.chymyst.ui.elm.{LabelAlignment, View}

import java.awt._
import java.awt.event.{ActionEvent, WindowAdapter, WindowEvent}
import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import javax.swing.{Box, BoxLayout}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

trait UiEventThread[V[_], E] extends UiBackend[V, E] with AutoCloseable {
  private val singleThreadExecutor = new ThreadPoolExecutor(1, 1, 1, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]())

  override def runOnEventThread[L](label: L, callback: E => Unit): E => Unit = { e =>
    if (EventQueue.isDispatchThread)
      callback(e)
    else
      singleThreadExecutor.execute(LabeledRunnable(label, () => EventQueue.invokeLater(() => callback(e))))
  }

  override def removePendingEvents[L](label: L): Unit = {
    singleThreadExecutor.getQueue.removeIf(t => t.isInstanceOf[LabeledRunnable[L]] && t.asInstanceOf[LabeledRunnable[L]].hasLabel(label))
  }

  override def close(): Unit = {
    singleThreadExecutor.shutdown()
  }
}

object UiEventThread {
  def createStandardBackend[V[_], E](render: V[E] => Consume[E]): UiBackend[V, E] = new UiBackend[V, E] with UiEventThread[V, E] {
    override def renderView: V[E] => Consume[E] = render
  }
}

object AwtRunner {
  private lazy val (frame, panel) = getFrameAndPanel()

  private def toAwtLabelAlignment: LabelAlignment => Int = {
    case LabelAlignment.Left => Label.LEFT
    case LabelAlignment.Center => Label.CENTER
    case LabelAlignment.Right => Label.RIGHT
  }

  private def getFrameAndPanel() = {
    val f = new Frame()
    f.setTitle("AWT Runner example")
    f.setBounds(400, 400, 640, 480)
    f.setLayout(new BoxLayout(f, BoxLayout.X_AXIS)) // TODO better layout, or just use Swing instead of AWT.
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

  // Return the first future that succeeds.
  private def first[T](f: Future[T], g: Future[T]): Future[T] = {
    val p = Promise[T]
    f.foreach { x => p.trySuccess(x) }
    g.foreach { x => p.trySuccess(x) }
    p.future
  }

  final case class LabeledRunnable[L](label: L, code: () => Unit) extends Runnable {
    def hasLabel(l: L): Boolean = label == l

    override def run(): Unit = code()
  }

  def backend[E]: UiBackend[View, E] = UiEventThread.createStandardBackend(AwtRunner.renderView)

  def renderView[E](view: View[E]): Consume[E] = {
    def render(subview: View[E], inPanel: => Panel = panel, clearPanel: Boolean = true): Consume[E] = consume => {
      //    println(s"DEBUG: adding view $view in panel $inPanel; panel is valid: ${inPanel.isValid}, on dispatch thread: ${EventQueue.isDispatchThread}")
      //    EventQueue.invokeLater { () =>
      if (clearPanel) inPanel.removeAll() //else println(s"DEBUG: Not clearing panel for view $view")
      subview match {
        case View.Label(text, alignment) =>
          inPanel.add(new Label(text, toAwtLabelAlignment(alignment)))

        case View.Button(text, event: E) =>
          val button = new Button(text)
          button.addActionListener { (_: ActionEvent) =>
            consume(event)
          }
          inPanel.add(button)

        case View.TileH(left, right) =>
          val n = new Panel()
          n.setLayout(new BoxLayout(n, BoxLayout.X_AXIS)) // We are using a Swing layout manager here, and it seems to be working.
          n.setName(s"Panel for $subview")
          //        n.setVisible(true) // Not necessary.
          inPanel.add(n)
          render(left, n, false)(consume)
          render(right, n, false)(consume)
          n.add(Box.createHorizontalGlue)

        case View.TileV(top, bottom) =>
          val n = new Panel()
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
