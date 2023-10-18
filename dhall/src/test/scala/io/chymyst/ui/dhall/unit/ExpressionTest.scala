package io.chymyst.ui.dhall.unit

import fastparse.Parsed
import io.chymyst.ui.dhall.Parser
import io.chymyst.ui.dhall.Syntax.DhallFile
import utest.{*, TestSuite, Tests, assertMatch, intercept, test}

object ExpressionTest extends TestSuite {
  override def tests: Tests = Tests {
    test("simple expression: 1+1") - {
      val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("1+1")
      // TODO: enable this test
      //      val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
      //      assert(result == expected)
    }

    test("simple expression: let x = 1 in x") - {
      val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall("let x = 1 in x")
      // TODO: enable this test
      //              val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
      //              assert(result == expected)
    }

    test("parse product.dhall") - {
      val Parsed.Success(DhallFile(Seq(), result), _) = Parser.parseDhall(getClass.getResourceAsStream("/product.dhall"))
      // TODO: enable this test
      //      val expected = Expression.Builtin(SyntaxConstants.Builtin.List)
      //      assert(result == expected)
    }

  }
}
