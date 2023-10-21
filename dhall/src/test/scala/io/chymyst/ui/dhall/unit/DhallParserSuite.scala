package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.{CBOR, Parser, Syntax}
import munit.FunSuite
import TestUtils._
import io.chymyst.ui.dhall.Syntax.DhallFile

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}
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
    val results = testFilesForSuccess.flatMap { file =>
      val r: Option[Syntax.Expression] = Try(Parser.parseDhall(new FileInputStream(file))).toOption.flatMap {
        case Parsed.Success(DhallFile(_, expr), _) => Some(expr)
        case _ => None
      }
      val result = r.map { expr => Try(CBOR.exprToBytes(expr)) }
      if (result.exists(_.isFailure)) println(s"Parsing or converting file ${file.getName} to CBOR failed: ${result.get.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

  test("validate CBOR encoding for standard examples") {
    val results = testFilesForSuccess.flatMap { file =>
      val validationFile = file.getAbsolutePath.replace("A.dhall", "B.dhallb")
      val cborValidationBytes = Files.readAllBytes(Paths.get(validationFile))
      val result1 = for {
        Parsed.Success(dhallValue, _) <- Try(Parser.parseDhall(new FileInputStream(file)))
        bytes <- Try(CBOR.exprToBytes(dhallValue.value))
      } yield bytes
      val result = result1.toOption.map { bytes =>
          if(bytes sameElements cborValidationBytes) Success(bytes)
          else Failure(new Exception(s"CBOR encoding differs"))
      }
      if (result.exists(_.isFailure)) println(s"CBOR validation failed for file ${file.getName}: ${result.get.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}")
  }

}
