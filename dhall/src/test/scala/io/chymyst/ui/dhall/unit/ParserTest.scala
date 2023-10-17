package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.{Grammar, Parser, SyntaxConstants}
import utest.{*, TestSuite, Tests, test, assert, intercept}
import fastparse._
//import com.eed3si9n.expecty.Expecty.assert
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}

object ParserTest extends TestSuite {
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

    test("parse product.dhall") - {
      val Parsed.Success(DhallFile(Seq(), result), lastIndex) = Parser.parseDhall(getClass.getResourceAsStream("/product.dhall"))
      val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
      assert(result == expected)
    }
  }
}
