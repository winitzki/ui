package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.runSimpleProgram
import io.chymyst.ui.elm.{ExampleSimpleElmProgram, ExampleSimpleElmProgram2, View}
import munit.FunSuite

class SimpleTest extends FunSuite {

  test("1") {
    runSimpleProgram[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, AwtRunner.renderView)
    Thread.sleep(10000L)
  }

  test("2") {
    runSimpleProgram[ExampleSimpleElmProgram2.M, View, ExampleSimpleElmProgram2.E](ExampleSimpleElmProgram2.program, AwtRunner.renderView)
    Thread.sleep(10000L)
  }
}
