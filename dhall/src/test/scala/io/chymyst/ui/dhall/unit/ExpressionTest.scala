package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse.Parsed
import io.chymyst.ui.dhall.Syntax.Expression.Variable
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}
import io.chymyst.ui.dhall.SyntaxConstants.{FieldName, VarName}
import io.chymyst.ui.dhall.{Parser, SyntaxConstants}
import munit.FunSuite

class ExpressionTest extends FunSuite {

  test("simple expression: 1+1") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("1+1")
    val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
    expect(result == expected, "1+1 must be parsed".nonEmpty)
  }

  test("simple expression: { foo, bar }") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("{ foo, bar }")
    val expected = Expression.RecordLiteral(List((FieldName("foo"), Variable(VarName("foo"), 0)), (FieldName("bar"), Variable(VarName("bar"), 0))))
    expect(result == expected, "{ foo, bar } must be parsed in the order foo, bar".nonEmpty)
  }

  test("simple expression: let x = 1 in x") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("let x = 1 in x")
    val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
    expect(result == expected)
  }

}
