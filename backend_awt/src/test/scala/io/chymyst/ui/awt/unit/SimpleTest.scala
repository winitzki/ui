package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.{SimpleProgram, runSimpleProgram}
import io.chymyst.ui.elm.{ExampleSimpleElmProgram, View}
import utest._

import scala.concurrent.ExecutionContext.Implicits.global

object SimpleTest extends TestSuite {

  val tests: Tests = this {
    test - {
      runSimpleProgram[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, AwtRunner.renderView)
      Thread.sleep(10000L)
    }
  }

}
