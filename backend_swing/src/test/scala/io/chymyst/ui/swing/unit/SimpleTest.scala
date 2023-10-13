package io.chymyst.ui.swing.unit

import io.chymyst.ui.swing.SwingRunner
import io.chymyst.ui.elm.Elm.runSimpleProgram
import io.chymyst.ui.elm.{ExampleSimpleElmProgram, View}
import utest._

object SimpleTest extends TestSuite {

  val tests: Tests = this {
    test - {
      runSimpleProgram[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, (new SwingRunner).renderView)
      Thread.sleep(10000L)
    }
  }

}
