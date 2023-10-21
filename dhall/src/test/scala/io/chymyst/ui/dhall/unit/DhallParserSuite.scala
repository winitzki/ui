package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.Parser
import munit.FunSuite
import TestUtils._
import java.io.{File, FileInputStream}
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

class DhallParserSuite extends FunSuite {

  test("load standard examples for successful parsing") {
    val testFiles = getClass.getClassLoader.getResource("parser-succeed").getPath.pipe(new File(_)).listFiles.toSeq
    val results = testFiles.filter(_.getName endsWith ".dhall").sortBy(_.getName).map { file =>
      val result = Try {
        val Parsed.Success(dhallValue, _) = Parser.parseDhall(new FileInputStream(file))
        dhallValue
      }
      if (result.isFailure) println(s"Parsing file ${file.getName} expecting success. Result: ${result.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

  test("load standard examples for failed parsing") {
    val testFiles = getClass.getClassLoader.getResource("parser-fail").getPath.pipe(new File(_)).listFiles.toSeq
    val results = testFiles.filter(_.getName endsWith ".dhall").sortBy(_.getName).map { file =>
      val result = Try {
        val Parsed.Success(result, _) = Parser.parseDhall(new FileInputStream(file))
        result
      }

      if (result.isSuccess) println(s"Parsing file ${file.getName} expecting failure. Result: unexpected success:\n\t\t\t${result.get}\n")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

}
