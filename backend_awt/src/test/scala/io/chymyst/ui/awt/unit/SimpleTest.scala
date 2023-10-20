package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.runSimpleProgram
import io.chymyst.ui.elm.{ExampleSimpleElmProgram, View}
import munit.FunSuite

object SimpleTest extends FunSuite {

  test("1") {
    runSimpleProgram[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, AwtRunner.renderView)
    Thread.sleep(10000L)
  }

}
