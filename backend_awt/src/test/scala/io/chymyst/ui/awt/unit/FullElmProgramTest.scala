package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.RunLoop
import io.chymyst.ui.elm.ExampleFullElmProgram
import munit.FunSuite

class FullElmProgramTest extends FunSuite {

  test("1") {
    val runLoop = new RunLoop(
      ExampleFullElmProgram.program,
    )(
      AwtRunner.backend,
      ExampleFullElmProgram.effectRunner,
    )
    runLoop.start()
    Thread.sleep(60000L)
  }

}
