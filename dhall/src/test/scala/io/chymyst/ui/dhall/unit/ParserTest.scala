package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.{Grammar, Parser, SyntaxConstants}
import utest.{TestSuite, Tests, test}
import fastparse._
import com.eed3si9n.expecty.Expecty.assert
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}

object ParserTest extends TestSuite {
  override def tests: Tests = Tests {

    test - {
      val Parsed.Success((), 1) = parse("asdf", Grammar.quoted_label_char(_))
      val f@Parsed.Failure(failure, index, _) = parse("`asdf", Grammar.quoted_label_char(_))
      assert(failure == "")
      assert(index == 0)
      assert(f.msg == """Position 1:1, found "`asdf"""")
    }

    test - {
      val Parsed.Success(result, 1) = Parser.parseDhall(getClass.getResourceAsStream("/product.dhall"))
      val expected: Expression = Expression.Builtin(SyntaxConstants.Builtin.List)
      assert(result == DhallFile(Seq(), expected))
    }
  }
}
