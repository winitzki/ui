package io.chymyst.ui.swing.unit

import io.chymyst.ui.swing.SwingRunner
import io.chymyst.ui.elm.Elm.runSimpleProgram
import io.chymyst.ui.elm.{ExampleSimpleElmProgram, ExampleSimpleElmProgram2, ExampleTextArea, View}
import munit.FunSuite

class SimpleTest extends FunSuite {

  test("1") {
    runSimpleProgram[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, (new SwingRunner).renderView)
    Thread.sleep(10000L)
  }

  test("2") {
    runSimpleProgram[ExampleSimpleElmProgram2.M, View, ExampleSimpleElmProgram2.E](ExampleSimpleElmProgram2.program, (new SwingRunner).renderView)
    Thread.sleep(10000L)
  }

  test("text area") {
    runSimpleProgram[ExampleTextArea.M, View, ExampleTextArea.E](ExampleTextArea.program, (new SwingRunner).renderView)
    Thread.sleep(10000L)
  }

}
