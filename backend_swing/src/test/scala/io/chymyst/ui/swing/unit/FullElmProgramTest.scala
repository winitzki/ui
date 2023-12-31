package io.chymyst.ui.swing.unit

import io.chymyst.ui.swing.SwingRunner
import io.chymyst.ui.awt.UiEventThread
import io.chymyst.ui.elm.Elm.RunLoop
import io.chymyst.ui.elm.{ExampleFullElmProgram, ExampleSimpleElmProgram2}
import munit.FunSuite

class FullElmProgramTest extends FunSuite {

  test("1") {
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
