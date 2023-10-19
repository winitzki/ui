package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.Parser
import utest.{TestSuite, Tests, test}

import java.io.{File, FileInputStream}
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

object DhallParserSuite extends TestSuite{
  override def tests: Tests = Tests {
    test("load standard examples for successful parsing") - {
      val testFiles = DhallParserSuite.getClass.getClassLoader.getResource("parser-succeed").getPath.pipe(new File(_)).listFiles.toSeq
      val results = testFiles.filter(_.getName endsWith ".dhall").sortBy(_.getName).map { file =>
        print(s"Parsing file ${file.getName} expecting success. Result: ")
        val result = Try {
          val Parsed.Success(dhallValue, _) = Parser.parseDhall(new FileInputStream(file))
          dhallValue
        }
        if (result.isSuccess) println("success.") else println("failure.")
        result
      }
      println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
    }

    test("load standard examples for failed parsing") - {
      val testFiles = DhallParserSuite.getClass.getClassLoader.getResource("parser-fail").getPath.pipe(new File(_)).listFiles.toSeq
      val results = testFiles.filter(_.getName endsWith ".dhall").sortBy(_.getName).map { file =>
        print(s"Parsing file ${file.getName} expecting failure. Result: ")
        val result = Try {
          val Parsed.Success(result, _) = Parser.parseDhall(new FileInputStream(file))
          result
        }
        if (result.isSuccess) println(" success.") else println(" failure.")
        result
      }
      println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
    }
  }
}
