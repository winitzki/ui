package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse.{Parsed, parse}
import io.chymyst.ui.dhall.Grammar.{equivalent_expression, import_alt_expression, whsp}
import io.chymyst.ui.dhall.Syntax.Expression.{NaturalLiteral, Variable}
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}
import io.chymyst.ui.dhall.SyntaxConstants.{FieldName, VarName}
import io.chymyst.ui.dhall.unit.TestUtils.{check, printFailure, toFail, v}
import io.chymyst.ui.dhall.{Grammar, Parser, SyntaxConstants}
import munit.FunSuite

import scala.util.Try

class SimpleExpressionTest extends FunSuite {

  test("simple invalid expression: 1+1") {
    toFail(Grammar.complete_dhall_file(_), "1+1", "", "", 1)
    //    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("1+1")
    //    val expected = Expression.NaturalLiteral(1)
    //    expect(result == expected, "1+1 must be parsed as 1".nonEmpty)
  }

  test("simple expression: { foo, bar }") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("{ foo, bar }")
    val expected = Expression.RecordLiteral(List(
      (FieldName("foo"), v("foo")),
      (FieldName("bar"), v("bar")),
    ))
    expect(result == expected, "{ foo, bar } must be parsed in the order foo, bar".nonEmpty)
  }

  test("simple expression: x") {
    expect(parse("x", Grammar.expression(_)).get.value == v("x"))
  }

  test("simple expression: let x = 1 in x with hand-written grammar") {
    expect(parse("1", Grammar.expression(_)).get.value == NaturalLiteral(1))
    expect(parse("x", Grammar.expression(_)).get.value == v("x"))
    expect(parse("let x = 1 ", Grammar.let_binding(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    TestUtils.toFail(Grammar.identifier(_), "in", "", "", 0)

    expect(parse("1 in", Grammar.application_expression(_)).get.value == NaturalLiteral(1))

    import fastparse._
    import NoWhitespace._
    def grammar1[$: P] = P(Grammar.let_binding)

    expect(parse("let x = 1 ", grammar1(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    def grammar2[$: P] = P(
      Grammar.let_binding ~ Grammar.requireKeyword("in")
    )

    expect(parse("let x=1 in ", grammar2(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))

    def grammar3[$: P] = (Grammar.let_binding ~ Grammar.requireKeyword("in"))

    expect(parse("let x = 1 in x", grammar3(_)).get.value == (VarName("x"), None, NaturalLiteral(1)))
  }

  test("simple expression: let x = 1 in y") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("let x = 1 in y")
    val expected = Expression.Let(VarName("x"), None, NaturalLiteral(1), v("y"))
    expect(result == expected)
  }

  test("simple expression: (x)") {
    val Parsed.Success(result, _) = parse("(x)", Grammar.primitive_expression(_))
    val expected = v("x")
    expect(result == expected)
  }

  test("parse a string interpolation") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall(""" "${1}" """)
    val expected = Expression.TextLiteral(List(("", Expression.NaturalLiteral(1))), "")
    expect(result == expected)
  }

  test("parse a sample file") {
    val testFile = getClass.getClassLoader.getResourceAsStream("parser-succeed/whitespaceBuffetA.dhall")
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall(testFile)
  }

  test("expression and a block comment") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("""1 {- -}""")
    val expected = Expression.NaturalLiteral(1)
    expect(result == expected)
  }

  test("expression and a line comment") {
    val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("""1 -- aaa \n""")
    val expected = Expression.NaturalLiteral(1)
    expect(result == expected)
  }

  test("expression followed by comment") {
    import fastparse._, NoWhitespace._
    val input = "x {- -}"
    val expected = v("x")

    def grammar1[$: P] = Grammar.whsp ~ Grammar.expression ~ Grammar.whsp

    val failures = Seq(
      Try(check(Grammar.whsp(_), " {- -}", ())),
      Try(check(Grammar.whsp(_), "", ())),
      Try(check(Grammar.expression(_), "x", expected)),
      Try(check(Grammar.primitive_expression(_), "x", expected)),
      Try(check(grammar1(_), "x {- -}", expected)),
    ).filter(_.isFailure).map(_.failed.get.getMessage)
    if (failures.nonEmpty) println(s"Found ${failures.size} failures:\n${failures.mkString("\n")}")
    expect(failures.isEmpty)
  }

  test("parse assert : x") {
    val input = "assert : x"
    val expected = Expression.Assert(v("x"))
    val Parsed.Success(result1, _) = parse(input, Grammar.expression_assert(_))
    expect(result1 == expected)
    val Parsed.Success(result2, _) = parse(input, Grammar.expression(_))
    expect(result2 == expected)
    val Parsed.Success(result3, _) = parse(input, Grammar.complete_expression(_))
    expect(result3 == expected)
    val Parsed.Success(result4, _) = parse(input, Grammar.complete_dhall_file(_))
    expect(result4.value == expected)
  }

  test("parse x === y") {
    import fastparse._, NoWhitespace._
    val input = "x === y"
    val expected = Expression.Operator(v("x"), SyntaxConstants.Operator.Equivalent, v("y"))

    check(Grammar.equivalent(_), "===", ())
    check(Grammar.import_alt_expression(_), "x", v("x"))

    // Expression "x === y" must be parsed as "x", leaving " === y" unconsumed!
    val failures = Seq(
      Try(check(Grammar.completion_expression(_), input, v("x"))),
      Try(check(Grammar.import_expression(_), input, v("x"))),
      Try(check(Grammar.first_application_expression(_), input, v("x"))),
      Try(check(Grammar.application_expression(_), input, v("x"))),
      Try(check(Grammar.not_equal_expression(_), input, v("x"))),
      Try(check(Grammar.equal_expression(_), input, v("x"))),
      Try(check(Grammar.times_expression(_), input, v("x"))),
      Try(check(Grammar.combine_types_expression(_), input, v("x"))),
      Try(check(Grammar.prefer_expression(_), input, v("x"))),
      Try(check(Grammar.combine_expression(_), input, v("x"))),
      Try(check(Grammar.and_expression(_), input, v("x"))),
      Try(check(Grammar.list_append_expression(_), input, v("x"))),
      Try(check(Grammar.text_append_expression(_), input, v("x"))),
      Try(check(Grammar.plus_expression(_), input, v("x"))),
      Try(check(Grammar.or_expression(_), input, v("x"))),
      Try(check(Grammar.import_alt_expression(_), input, v("x"))),
    ).filter(_.isFailure).map(_.failed.get).map(printFailure).mkString("\n\n")
    if (failures.nonEmpty) println(s"ERROR failures = $failures")

    val Parsed.Success(result1, _) = parse(input, Grammar.equivalent_expression(_))
    expect(result1 == expected)
  }

  test("parse assert : x === y") {
    val input = "assert : x === y"
    val expected = Expression.Assert(Expression.Operator(v("x"), SyntaxConstants.Operator.Equivalent, v("y")))
    val Parsed.Success(result1, _) = parse(input, Grammar.expression_assert(_))
    expect(result1 == expected)
    val Parsed.Success(result2, _) = parse(input, Grammar.expression(_))
    expect(result2 == expected)
    val Parsed.Success(result3, _) = parse(input, Grammar.complete_expression(_))
    expect(result3 == expected)
    val Parsed.Success(result4, _) = parse(input, Grammar.complete_dhall_file(_))
    expect(result4.value == expected)
  }

  test("simple_label") {
    val input = "witha"
    check(Grammar.simple_label(_), input, ())
  }

  test("empty record literal") {
    check(Grammar.complete_expression(_),  "{ }", Expression.RecordType(Seq()))
    check(Grammar.complete_expression(_),  "{=}", Expression.RecordLiteral(Seq()))
    check(Grammar.complete_expression(_), "{,}", Expression.RecordType(Seq()))
    check(Grammar.complete_expression(_), "{}", Expression.RecordType(Seq()))
  }

  test("invalid utf-8") {
    val input = Array[Byte](-19, -96, -128)
    import fastparse._, NoWhitespace._
    def grammar[$: P] = P(SingleChar.rep)

    val result = parse(input, grammar(_))
    result.get.value.foreach(s => println(s"Char: ${s.toInt}"))
    // TODO: figure out how `fastparse` decodes a byte array into characters. This test shows that the non-UTF8 sequence is decoded as a single character 65533.
  }

}
