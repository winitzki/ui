package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.{RunLoop, run}
import io.chymyst.ui.elm.View
import utest.{TestSuite, test}

class FullElmProgramTest extends TestSuite {

  val tests = this {
    test - {
      val runLoop = new RunLoop[ExampleFullElmProgram.M, View, ExampleFullElmProgram.E, ExampleFullElmProgram.C, ExampleFullElmProgram.S](
        ExampleFullElmProgram.program,
        AwtRunner.renderView,
        ExampleFullElmProgram.runCommand,
        ExampleFullElmProgram.listen,
      )
      runLoop.start()
    }
  }
}
