package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.RunLoop
import io.chymyst.ui.elm.{ExampleFullElmProgram, View}
import utest.{TestSuite, test}

object FullElmProgramTest extends TestSuite {

  val tests = this {
    test - {
      val runLoop = new RunLoop[ExampleFullElmProgram.M, View, ExampleFullElmProgram.E, ExampleFullElmProgram.C, ExampleFullElmProgram.S](
        ExampleFullElmProgram.program,
        AwtRunner.backend[ExampleFullElmProgram.E],
        ExampleFullElmProgram.runCommand,
        ExampleFullElmProgram.listen,
      )
      runLoop.start()
      Thread.sleep(60000L)
    }
  }
}
