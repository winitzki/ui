package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.{CBOR, Parser, Syntax}
import munit.FunSuite
import TestUtils._
import com.eed3si9n.expecty.Expecty.expect
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}

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
      val result = for {
        result1 <- Try(Parser.parseDhall(new FileInputStream(file)))
          .recoverWith { case exception => Failure(new Exception(s"Parsing file ${file.getName} expecting success. Result: parser crashed with: ${printFailure(exception)}")) }
        result2 <- result1 match {
          case Parsed.Success(value, index) => Success(value)
          case Parsed.Failure(a, b, c) => Failure(new Exception(s"Parsing file ${file.getName} expecting success. Result: $result1, diagnostics: ${c.stack}"))
        }
      } yield result2
      result match {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => ()
      }
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
    val failures = results.count(_.isSuccess) // We expect that all examples fail to parse here.
    println(s"Success count: ${results.count(_.isFailure)}\nFailure count: $failures")
    expect(failures == 0)
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
    expect(results.count(_.isFailure) == 0)
  }

  test("validate CBOR encoding for standard examples") {
    val outDir = "./testdhallb"
    Try(Files.createDirectory(Paths.get(outDir)))
    val results = testFilesForSuccess.flatMap { file =>
      val validationFile = file.getAbsolutePath.replace("A.dhall", "B.dhallb")
      val cborValidationBytes = Files.readAllBytes(Paths.get(validationFile))
      val cborValidationModel = CBORtest.bytesToCBORmodel(cborValidationBytes).toString
      val diagnosticFile = file.getAbsolutePath.replace("A.dhall", "B.diag")
      val diagnosticString = Files.readString(Paths.get(diagnosticFile)).trim
      val result1 = for {
        Parsed.Success(dhallValue, _) <- Try(Parser.parseDhall(new FileInputStream(file)))
        model <- Try(CBOR.toCborModel(dhallValue.value))
        bytesGeneratedByUs <- Try(model.toCBOR.EncodeToBytes())
      } yield (model, bytesGeneratedByUs, dhallValue.value)
      val result = result1.toOption.map { case (model, bytesGeneratedByUs, expression) =>
        Files.write(Paths.get(outDir + "/" + file.getName.replace("A.dhall", "A.dhallb")), bytesGeneratedByUs)
        if (bytesGeneratedByUs sameElements cborValidationBytes) Success(bytesGeneratedByUs)
        else if (model.toString == diagnosticString) {
          val extraMessage = if (model.toString != cborValidationModel) s"\nwhile our reading of the validation file also differs:\n\t\t$cborValidationModel" else ""
          Failure(new Exception(s"CBOR encoding differs, our expression is '$expression', but generated CBOR model agrees with expected:\n\t\t$model$extraMessage\n"))
        } else Failure(new Exception(s"CBOR model differs: our CBOR model is:\n$model\nbut expected CBOR model is:\n$diagnosticString\n"))
      }
      if (result.exists(_.isFailure)) println(s"CBOR validation failed for file ${file.getName}: ${result.get.failed.get.getMessage}")
      result
    }
    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}\nCBOR model mismatch count: ${results.filter(_.isFailure).count(_.failed.get.getMessage.contains("model differs"))}")
  }

  test("validate CBOR decoding for standard examples") {
    val results = testFilesForSuccess.flatMap { file =>
      val validationFile = file.getAbsolutePath.replace("A.dhall", "B.dhallb")
      val cborValidationBytes = Files.readAllBytes(Paths.get(validationFile))
      val cborValidationModel = CBORtest.bytesToCBORmodel(cborValidationBytes).toString
      val diagnosticFile = file.getAbsolutePath.replace("A.dhall", "B.diag")
      val diagnosticString = Files.readString(Paths.get(diagnosticFile)).trim
      val result1 = for {
        Parsed.Success(dhallValue, _) <- Try(Parser.parseDhall(new FileInputStream(file)))
        model <- Try(CBOR.toCborModel(dhallValue.value))
        bytesGeneratedByUs <- Try(model.toCBOR.EncodeToBytes())
      } yield (model, bytesGeneratedByUs, dhallValue.value)
      val result2 = result1.toOption.map { case (model, bytesGeneratedByUs, expression) =>
        if (bytesGeneratedByUs sameElements cborValidationBytes) Success((model, expression))
        else if (model.toString == diagnosticString) {
          val extraMessage = if (model.toString != cborValidationModel) s"\nwhile our reading of the validation file also differs:\n\t\t$cborValidationModel" else ""
          Failure(new Exception(s"CBOR encoding differs, our expression is '$expression', but generated CBOR model agrees with expected:\n\t\t$model$extraMessage\n"))
        } else Failure(new Exception(s"CBOR model differs: our CBOR model is:\n$model\nbut expected CBOR model is:\n$diagnosticString\n"))
      }.flatMap(_.toOption)
      result2.map { case (model, expression) =>
        Try(model.toExpression == expression) match {
          case Failure(exception) => Failure(new Exception(s"File ${file.getName}: Parser crashed on model $model: $exception"))
          case Success(true) => Success(true)
          case Success(false) => Failure(new Exception(s"File ${file.getName}: After restoring from bytes, expression differs: expected $expression but got ${model.toExpression}"))
        }
      }

    }

    println(s"Success count: ${results.count(_.isSuccess)}\nFailure count: ${results.count(_.isFailure)}\nCBOR expression mismatch count: ${results.filter(_.isFailure).count(_.failed.get.getMessage.contains("expression differs"))}")
    results.filter(_.isFailure).map(_.failed.get.getMessage).foreach(println)
  }
}
