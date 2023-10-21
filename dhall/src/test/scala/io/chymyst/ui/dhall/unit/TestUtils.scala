package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse._

import java.io.{PrintWriter, StringWriter}
import scala.util.Try

object TestUtils {

  def printFailure(t: Throwable): String = {
    val stackTrace = new StringWriter
    t.printStackTrace(new PrintWriter(stackTrace))
    stackTrace.flush()
    //        t.getMessage + "\n\n" + // No need to print the message because the stack trace already contains all that.
    stackTrace.toString
  }

  def check[A](grammarRule: P[_] => P[A], input: String, expectedResult: A, lastIndex: Int): Unit = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Parsing input '$input', got Success($value, $index), expecting Success($expectedResult, $lastIndex)")
      case Parsed.Failure(message, index, extra) =>
        println(s"Error: Parsing input '$input', expected Success($expectedResult, $index) but got Failure('$message', $index, ${extra.stack})")
    }
    expect((input != null) && (parsed == Parsed.Success(expectedResult, lastIndex)))
  }

  def toFail[A](grammarRule: P[_] => P[A], input: String, parsedInput: String, expectedMessage: String, lastIndex: Int): Unit = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Error: Parsing input '$input', expected Failure but got Success($value, $index)")
        expect(parsed == Parsed.Failure(parsedInput, lastIndex, null))
      case f@Parsed.Failure(message, index, extra) =>
        println(s"Parsing input '$input', expected index $lastIndex, got Failure('$message', $index, ${extra.stack}), message '${f.msg}' as expected")
        expect(input != null && (f.msg contains expectedMessage), input != null && f.index == lastIndex)
    }
  }

  def check[A](successExamples: Seq[(String, A)], grammarRule: P[_] => P[A]): Unit = {
    val results = successExamples.map { case (s, d) =>
      Try(check(grammarRule(_), s, d, s.length))
    }
    if (results.forall(_.isSuccess))
      println(s"All ${successExamples.size} examples passed.")
    else {
      println(s"Error: ${results.count(_.isFailure)} examples failed:")
      val message = results.filter(_.isFailure).map(_.failed.get).map(printFailure).mkString("\n\n")
      throw new Exception(message)
    }
  }

}
