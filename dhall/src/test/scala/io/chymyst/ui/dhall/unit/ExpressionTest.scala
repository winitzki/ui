package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse.{Parsed, parse}
import io.chymyst.ui.dhall.Syntax.Expression.{NaturalLiteral, Variable}
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}
import io.chymyst.ui.dhall.SyntaxConstants.{FieldName, VarName}
import io.chymyst.ui.dhall.{Grammar, Parser, SyntaxConstants}
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

  test("simple expression: x") {
    expect(parse("x", Grammar.expression(_)).get.value == Expression.Variable(VarName("x"), BigInt(0)))
  }

  test("simple expression: let x = 1 in x with hand-written grammar") {
    expect(parse("1", Grammar.expression(_)).get.value == NaturalLiteral(1))
    expect(parse("x", Grammar.expression(_)).get.value == Expression.Variable(VarName("x"), BigInt(0)))
    expect(parse("let x = 1 ", Grammar.let_binding(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    //    expect(parse("in", Grammar.identifier(_)).get.value == NaturalLiteral(1)) // this parse must fail.

    expect(parse("1 in", Grammar.application_expression(_)).get.value == NaturalLiteral(1))

    import fastparse._, NoWhitespace._
    def grammar1[$: P] = P(Grammar.let_binding)

    expect(parse("let x = 1 ", grammar1(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    def grammar2[$: P] = P(
      Grammar.let_binding.log.logBefore("letb-").logAfter("letb+")
        ~ Grammar.requireKeyword("in").log
    )

    expect(parse("let x=1 in ", grammar2(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    def grammar3[$: P] = (Grammar.let_binding ~ Grammar.requireKeyword("in").log.logBefore("in-").logAfter("in+"))

    expect(parse("let x = 1 in x", grammar3(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))
  }


  test("simple expression: let x = 1 in x") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("let x = 1 in x")
    val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
    expect(result == expected)
  }

}
