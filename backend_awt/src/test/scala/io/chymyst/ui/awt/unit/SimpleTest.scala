package io.chymyst.ui.awt.unit

import io.chymyst.ui.awt.AwtRunner
import io.chymyst.ui.elm.Elm.{SimpleProgram, run}
import io.chymyst.ui.elm.View
import utest._

import scala.concurrent.ExecutionContext.Implicits.global

object SimpleTest extends TestSuite {

  val tests: Tests = this {
    test - {
      run[ExampleSimpleElmProgram.M, View, ExampleSimpleElmProgram.E](ExampleSimpleElmProgram.program, AwtRunner.renderView)
    }
  }

}
