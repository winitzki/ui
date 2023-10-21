package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.{CBOR, Parser}
import munit.FunSuite
import TestUtils._

import java.io.{File, FileInputStream}
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

class DhallParserSuite extends FunSuite {

  def enumerateResourceFiles(directory: String, filterBySuffix: Option[String] = None): Seq[File] =
    getClass.getClassLoader.getResource(directory)
      .getPath
      .pipe(new File(_)).listFiles.toSeq
      .filter(f => filterBySuffix.forall(suffix => f.getName.endsWith(suffix)))
      .sortBy(_.getName)

  def testFilesForSuccess = enumerateResourceFiles("parser-succeed", Some(".dhall"))

  def testFilesForFailure = enumerateResourceFiles("parser-fail", Some(".dhall"))

  test("parse standard examples for successful parsing") {
    val results = testFilesForSuccess.map { file =>
      val result = Try {
        val Parsed.Success(dhallValue, _) = Parser.parseDhall(new FileInputStream(file))
        dhallValue
      }
      if (result.isFailure) println(s"Parsing file ${file.getName} expecting success. Result: ${result.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

  test("parse standard examples for failed parsing") {
    val results = testFilesForFailure.map { file =>
      val result = Try {
        val Parsed.Success(result, _) = Parser.parseDhall(new FileInputStream(file))
        result
      }

      if (result.isSuccess) println(s"Parsing file ${file.getName} expecting failure. Result: unexpected success:\n\t\t\t${result.get}\n")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

  test("convert standard examples for successful parsing into CBOR") {
    val results = testFilesForSuccess.map { file =>
      val result = for {
        Parsed.Success(dhallValue, _) <- Try(Parser.parseDhall(new FileInputStream(file)))
        bytes <- Try(CBOR.exprToBytes(dhallValue.value))
      } yield bytes
      if (result.isFailure) println(s"Parsing or converting file ${file.getName} to CBOR failed: ${result.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

}
