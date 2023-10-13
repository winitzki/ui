package io.chymyst.ui.swing.unit

import io.chymyst.ui.swing.SwingRunner
import io.chymyst.ui.awt.UiEventThread
import io.chymyst.ui.elm.Elm.RunLoop
import io.chymyst.ui.elm.ExampleFullElmProgram
import utest.{TestSuite, test}

object FullElmProgramTest extends TestSuite {

  val tests = this {
    test - {
      val runLoop = new RunLoop(
        ExampleFullElmProgram.program,
      )(
        UiEventThread.createStandardBackend((new SwingRunner).renderView),
        ExampleFullElmProgram.effectRunner,
      )
      runLoop.start()
      Thread.sleep(60000L)
    }
  }
}
