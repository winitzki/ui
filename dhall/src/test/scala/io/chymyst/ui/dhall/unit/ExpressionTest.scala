package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse.Parsed
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}
import io.chymyst.ui.dhall.{Parser, SyntaxConstants}
import munit.FunSuite

class ExpressionTest extends FunSuite {

  test("simple expression: 1+1") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("1+1")
    val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
    expect(result == expected, "1+1 must be parsed".nonEmpty)
  }

  test("simple expression: let x = 1 in x") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("let x = 1 in x")
    // TODO: enable this test
    //              val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
    //              expect(result == expected)
  }

}
