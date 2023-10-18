package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.assert
import fastparse._
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.Syntax.Expression.TextLiteral
import io.chymyst.ui.dhall.SyntaxConstants.VarName
import io.chymyst.ui.dhall.unit.TestFixtures._
import io.chymyst.ui.dhall.{Grammar, SyntaxConstants}
import utest.{TestSuite, Tests, intercept, test}

import java.nio.file.{Files, Paths}

object ParserTest extends TestSuite {

  def check[A](grammarRule: P[_] => P[A], input: String, expectedResult: A, lastIndex: Int) = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Parsing input '$input', got Success($value, $index), expecting Success($expectedResult, $lastIndex)")
      case Parsed.Failure(message, index, extra) =>
        println(s"Error: Parsing input '$input', expected Success($expectedResult, $index) but got Failure('$message', $index, ${extra.stack})")
    }
    assert(parsed == Parsed.Success(expectedResult, lastIndex))
  }

  def toFail[A](grammarRule: P[_] => P[A], input: String, parsedInput: String, expectedMessage: String, lastIndex: Int) = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Error: Parsing input '$input', expected Failure but got Success($value, $index)")
        assert(parsed == Parsed.Failure(parsedInput, lastIndex, null))
      case f@Parsed.Failure(message, index, extra) =>
        println(s"Parsing input '$input', expected index $lastIndex, got Failure('$message', $index, ${extra.stack}), message '${f.msg}' as expected")
        assert((f.msg contains expectedMessage) && (f.index == lastIndex))
    }
  }

  override def tests: Tests = Tests {

    test("quoted_label_char") - {
      val Parsed.Success((), 1) = parse("asdf", Grammar.quoted_label_char(_))
      val f@Parsed.Failure(failure, index, _) = parse("`asdf", Grammar.quoted_label_char(_))
      assert(failure == "")
      assert(index == 0)
      assert(f.msg == """Position 1:1, found "`asdf"""")
    }

    test("requireKeyword") - {
      val Parsed.Success(_, 5) = parse("merge", Grammar.requireKeyword("merge")(_))
      intercept[AssertionError] {
        parse("blah", Grammar.requireKeyword("blah")(_))
      }
    }

    test("end_of_line") - {
      check(Grammar.end_of_line(_), "\n\n\n", (), 1)
      check(Grammar.end_of_line(_), "\r\n\n", (), 2)
      check(Grammar.end_of_line(_), "\n\r\n", (), 1)
    }

    test("valid_non_ascii") - {
      toFail(Grammar.valid_non_ascii(_), "\n\n\n", "", " found ", 0)
      toFail(Grammar.valid_non_ascii(_), "", "", " found \"\"", 0)
      toFail(Grammar.valid_non_ascii(_), "abc", "", " found ", 0)

      "_ -`~,<?/\\\b".foreach { c =>
        toFail(Grammar.valid_non_ascii(_), s"$c", "", " found ", 0)
      }

      check(Grammar.valid_non_ascii(_), "ф  ", (), 1)
      "¡™£¢∞§¶•ªº≠œ∑´®†¥¨ˆøπ“‘«åß∂ƒ©˙∆˚¬…æ≥≤µ˜∫√ç≈Ω⁄€‹›ﬁ°·‚—±»’”∏Ø¨ÁÅÍÎÏÓÔÒÚÆ˘¯Â¸фывапролджэאבגדהוזחטיכךלמםנןסעפףצץקרששׂשׁתבּגּדּהּוּוֹזּטּיּכּךּךָךְלּמּנּסּפּףּצּקּשּׂשּׁתּ".foreach { c =>
        check(Grammar.valid_non_ascii(_), s"$c", (), 1)
      }

    }

    test("valid_non_ascii with large Unicode values from UTF-16 surrogates") - {
      check(Grammar.valid_non_ascii(_), "\uD800\uDC02", (), 2)
      check(Grammar.valid_non_ascii(_), "\uD83E\uDFFF", (), 2)
      check(Grammar.valid_non_ascii(_), "\uD83F\uDFFD", (), 2)
      toFail(Grammar.valid_non_ascii(_), "\uD83F\uDFFE", "", " found ", 0)
      toFail(Grammar.valid_non_ascii(_), "\uD83F\uDFFF", "", " found ", 0)
    }

    test("valid_non_ascii with large Unicode values from file") - {
      import fastparse._
      import NoWhitespace._

      val input: String = Files.readString(Paths.get(ParserTest.getClass.getResource("/valid_non_ascii.txt").toURI))

      def rule[$: P] = P(Grammar.valid_non_ascii.rep)

      check(rule(_), input, (), 8)
    }

    test("tab") - {
      check(Grammar.tab(_), "\t", (), 1)
      toFail(Grammar.tab(_), "\n\t", "", " found ", 0)
    }

    test("block_comment") - {
      blockComments.foreach { input =>
        check(Grammar.block_comment(_), input, (), input.length)
      }
    }

    test("whsp") - {
        (blockComments ++ multilineComments).foreach { input =>
        check(Grammar.whsp(_), input, (), input.length)
      }
    }

    test("whsp fails when not closed") - {
      toFail(Grammar.whsp(_), "{-", "", "", 2)
      toFail(Grammar.whsp(_), "{- {- -} -0", "", "", 8)

    }

    test("whsp fails when incomplete") - {
      // Nothing gets parsed.
      Seq( // Examples may contain trailing whitespace or leading whitespace.
        "",
        "фыва3 ç≈Ω⁄€‹›ﬁ° }}-}"
      ).foreach { input =>
        check(Grammar.whsp(_), input, (), 0)
      }

      Seq( // Examples may contain trailing whitespace or leading whitespace.
        "   {- 1 - }- }  ",
        """   {-2
          | - }-
          |}  |
          |- }""".stripMargin,
      ).foreach { input =>
        toFail(Grammar.whsp(_), input, "", "", 5)
      }

    }

    test("whitespace_chunk") - {
      Seq( // Examples should not contain trailing whitespace or leading whitespace.
        "{- - }- } -}",
        """{-
          | - }-
          |}  |
          |-}""".stripMargin,
        "{-фыва ç≈Ω⁄€‹›ﬁ° }}-}",
        "{--}",
        "{-{--}-}",
        " ",
        "--\n",
        "-- \n",
      ).foreach { input =>
        check(Grammar.whitespace_chunk(_), input, (), input.length)
      }
      check(Grammar.whitespace_chunk(_), " -- \n", (), 1)
    }

    test("whsp1") - {
      Seq( // Examples should not contain trailing whitespace or leading whitespace.
        "{- - }- } -}",
        """{-
          | - }-
          |}  |
          |-}""".stripMargin,
        "{-фыва ç≈Ω⁄€‹›ﬁ° }}-}",
        "{--}",
        "{-{--}-}",
        " ",
        " -- \n",
        "-- \n  ",
        " --{-\n",
        "--\n",
      ).foreach { input =>
        check(Grammar.whsp1(_), input, (), input.length)
      }
    }

    test("keyword") - {
      Grammar.simpleKeywords.foreach { input =>
        check(Grammar.keyword(_), input, input, input.length)
        check(Grammar.keyword(_), input + " ", input, input.length)
        check(Grammar.keyword(_), input + "(", input, input.length)
      }

      check(Grammar.forall(_), "forall", (), 6)
      check(Grammar.forall(_), "∀ ", (), 1)
    }

    test("simple_label") - {
      Seq(
        "abcd",
        "witha",
        "awith",
        "if_",
        "asa",
        "_in",
        "asif",
        "forallx",
      ).foreach { input =>
        check(Grammar.simple_label(_), input, (), input.length)
      }
      check(Grammar.simple_label(_), "x∀ ", (), 1)
      toFail(Grammar.simple_label(_), "∀", "", "", 0)
      toFail(Grammar.simple_label(_), "∀x", "", "", 0)
    }

    test("builtin names") - {
      val names = SyntaxConstants.Builtin.namesToValuesMap
      assert(names.keySet.size == 42)
      names.foreach { case (name, c) =>
        check(Grammar.builtin(_), name, Expression.Builtin(c), name.length)
      }
    }

    test("numeric_double_literal") - {
      Map(
        "1.0" -> 1.0,
        "-1.0" -> -1.0,
        "1.2" -> 1.2,
        "1.5" -> 1.5,
        "100e2" -> 100e2,
        "-100e12" -> -100e12,
        "100e-2" -> 100e-2,
        "1.0e-3" -> 1.0e-3,
      ).foreach { case (s, d) =>
        check(Grammar.numeric_double_literal(_), s, Expression.DoubleLiteral(d), s.length)
      }
    }

    test("natural_literal") - {
      Map(
        "9" -> BigInt(9),
        "1234512345123451234512345123451234512345" -> BigInt("1234512345123451234512345123451234512345"),
        "0" -> BigInt(0),
        "0x10" -> BigInt(16),
        "0xFFFF" -> BigInt(65535),
      ).foreach { case (s, d) =>
        check(Grammar.natural_literal(_), s, Expression.NaturalLiteral(d), s.length)
      }
      // Leading zero digits are not allowed.
      check(Grammar.natural_literal(_), "00001", Expression.NaturalLiteral(BigInt(0)), 1)
    }

    test("integer_literal") - {

      Map(
        "+9" -> BigInt(9),
        "+1234512345123451234512345123451234512345" -> BigInt("1234512345123451234512345123451234512345"),
        "+0" -> BigInt(0),
        "+0x10" -> BigInt(16),
        "+0xFFFF" -> BigInt(65535),
        "-9" -> BigInt(-9),
        "-1234512345123451234512345123451234512345" -> BigInt("-1234512345123451234512345123451234512345"),
        "-0" -> BigInt(0),
        "-0x10" -> BigInt(-16),
        "-0xFFFF" -> BigInt(-65535),
      ).foreach { case (s, d) =>
        check(Grammar.integer_literal(_), s, Expression.IntegerLiteral(d), s.length)
      }
      // Leading zero digits are not allowed.
      check(Grammar.integer_literal(_), "+00001", Expression.IntegerLiteral(BigInt(0)), 2)
      check(Grammar.integer_literal(_), "-00001", Expression.IntegerLiteral(BigInt(0)), 2)
      // Either plus or minus sign is required.
      toFail(Grammar.integer_literal(_), "0", "", "", 0)
      toFail(Grammar.integer_literal(_), "1", "", "", 0)
      toFail(Grammar.integer_literal(_), " ", "", "", 0)
    }

    // TODO: tests for date and time literals

    test("identifier") - {
      Seq(
        "Natural-blahblah" -> Expression.Variable(VarName("Natural-blahblah"), BigInt(0)),
        "Natural/blahblah" -> Expression.Variable(VarName("Natural/blahblah"), BigInt(0)),
        "Natural/show123" -> Expression.Variable(VarName("Natural/show123"), BigInt(0)),
        "abc" -> Expression.Variable(VarName("abc"), BigInt(0)),
        "a-b/c" -> Expression.Variable(VarName("a-b/c"), BigInt(0)),
        "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
        "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
        "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
        "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
      ).foreach { case (s, d) =>
        check(Grammar.identifier(_), s, d, s.length)
      }

      check(Grammar.identifier(_), "Natural+blahblah", Expression.Builtin(SyntaxConstants.Builtin.Natural), 7)
      toFail(Grammar.identifier(_), "-abc", "", "", 0)
      toFail(Grammar.identifier(_), "/abc", "", "", 0)
    }

    test("bytes_literal") - {
      val Parsed.Success(result, 12) = parse( "0x\"64646464\"", Grammar.bytes_literal(_))
      assert(new String(result.value) == "dddd")
    }

    test("primitive_expression") - {
      Seq(
        "12345" -> Expression.NaturalLiteral(BigInt(12345)),
        "-4312.2" -> Expression.DoubleLiteral(-4312.2),
        "\"123\"" -> TextLiteral.ofText(Expression.TextLiteralNoInterp("123")),
        """''
          |line
          |''""".stripMargin -> TextLiteral.ofText(Expression.TextLiteralNoInterp("line\n")),
        "x" -> Expression.Variable(VarName("x"), BigInt(0)),
        "(x)" -> Expression.Variable(VarName("x"), BigInt(0)),
        "( x )" -> Expression.Variable(VarName("x"), BigInt(0)),
        "( -12345  )" -> Expression.IntegerLiteral(BigInt(-12345)),
        "a-b/c" -> Expression.Variable(VarName("a-b/c"), BigInt(0)),
        "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
        "[1,2,3]" -> Expression.NonEmptyList(Expression.NaturalLiteral(BigInt(1)), Seq(Expression.NaturalLiteral(BigInt(2)), Expression.NaturalLiteral(BigInt(3)))),
        "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
        "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
        "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
      ).foreach { case (s, d) =>
        check(Grammar.primitive_expression(_), s, d, s.length)
      }

      val Parsed.Success(Expression.BytesLiteral(result), 12) = parse("0x\"64646464\"", Grammar.primitive_expression(_))
      assert(new String(result) == "dddd")
    }

    test("import_expression") - {
      Seq(
        "12345" -> Expression.NaturalLiteral(BigInt(12345)),
        "-4312.2" -> Expression.DoubleLiteral(-4312.2),
        "\"123\"" -> TextLiteral.ofText(Expression.TextLiteralNoInterp("123")),
        """''
          |line
          |''""".stripMargin -> TextLiteral.ofText(Expression.TextLiteralNoInterp("line\n")),
        "x" -> Expression.Variable(VarName("x"), BigInt(0)),
        "(x)" -> Expression.Variable(VarName("x"), BigInt(0)),
        "( x )" -> Expression.Variable(VarName("x"), BigInt(0)),
        "( -12345  )" -> Expression.IntegerLiteral(BigInt(-12345)),
        "a-b/c" -> Expression.Variable(VarName("a-b/c"), BigInt(0)),
        "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
        "[1,2,3]" -> Expression.NonEmptyList(Expression.NaturalLiteral(BigInt(1)), Seq(Expression.NaturalLiteral(BigInt(2)), Expression.NaturalLiteral(BigInt(3)))),
        "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
        "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
        "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
      ).foreach { case (s, d) =>
        check(Grammar.import_expression(_), s, d, s.length)
      }
    }

  }
}
