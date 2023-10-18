package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.{Grammar, Parser, SyntaxConstants}
import utest.{*, TestSuite, Tests, assertMatch, intercept, test}
import fastparse._
import com.eed3si9n.expecty.Expecty.assert
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}

import java.nio.file.{Files, Paths}

object ParserTest extends TestSuite {

  override def tests: Tests = Tests {

    test("basic tests") - {
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

      test("parse product.dhall") - {
        val Parsed.Success(DhallFile(Seq(), result), lastIndex) = Parser.parseDhall(getClass.getResourceAsStream("/product.dhall"))
        // TODO: enable this test
        //      val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
        //      assert(result == expected)
      }
    }

    //    test("parser rule") - {
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
      import fastparse._, NoWhitespace._

      val input: String = Files.readString(Paths.get(ParserTest.getClass.getResource("/valid_non_ascii.txt").toURI))

      def rule[$: P] = P(Grammar.valid_non_ascii.rep)

      check(rule(_), input, (), 8)
    }

    test("tab") - {
      check(Grammar.tab(_), "\t", (), 1)
      toFail(Grammar.tab(_), "\n\t", "", " found ", 0)
    }

    test("block_comment") - {
      Seq( // Examples should not contain trailing whitespace or leading whitespace.
        """{- }- -}""",
        "{-фыва ç≈Ω⁄€‹›ﬁ° }}-}",
      ).foreach { input =>
        check(Grammar.block_comment(_), input, (), input.length)
      }
    }
    //    }
  }

  def check[A](grammarRule: P[_] => P[A], input: String, expectedResult: A, lastIndex: Int) = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Parsing input '$input', got Success($value, $index)")
      case Parsed.Failure(message, index, extra) =>
        println(s"Error: Parsing input '$input', expected Success but got Failure('$message', $index, ${extra.stack})")
    }
    assertMatch(parsed) { case Parsed.Success(`expectedResult`, `lastIndex`) => }
  }

  def toFail[A](grammarRule: P[_] => P[A], input: String, parsedInput: String, expectedMessage: String, lastIndex: Int) = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Error: Parsing input '$input', expected Failure but got Success($value, $index)")
      case f@Parsed.Failure(message, index, extra) =>
        println(s"Parsing input '$input', got Failure('$message', $index, ${extra.stack}), message '${f.msg}' as expected")
    }
    assertMatch(parsed) { case f@Parsed.Failure(`parsedInput`, `lastIndex`, extra) if f.msg contains expectedMessage => }
  }
}
