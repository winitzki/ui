package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.Syntax.Expression.TextLiteral
import io.chymyst.ui.dhall.SyntaxConstants
import io.chymyst.ui.dhall.SyntaxConstants.{FieldName, VarName}

object TestFixtures {

  val blockComments = Seq( // Examples should not contain trailing whitespace or leading whitespace.
    "{- - }- } -}",
    """{-
      | - }-
      |}  |
      |-}""".stripMargin,
    "{-фыва ç≈Ω⁄€‹›ﬁ° }}-}",
    "{--}",
    "{-{--}-}",
    "{-{--}--}",
  )

  val multilineComments = Seq( // Examples may contain trailing whitespace or leading whitespace.
    """ -- {-
      | {-
      | }- -}
      |""".stripMargin,
    """
      |
      |     -- asl;dkjfalskdjфыва ç≈Ω⁄€‹›ﬁ°flakj
      |
      |     {-
      |
      |     фыва ç≈Ω⁄€‹›ﬁ°
      |
      |     --  -}
      |
      |     """.stripMargin
  )

  val whitespaceComments1 = Seq( // Examples should not contain trailing whitespace or leading whitespace.
    " ",
    "--\n",
    "-- \n",
  )

  val whitespaceCommentsWithLeadingSpace = Seq(
    " -- \n",
    " --{-\n",
  )

  val identifiers = Seq(
    "Natural-blahblah" -> Expression.Variable(VarName("Natural-blahblah"), BigInt(0)),
    "Natural/blahblah" -> Expression.Variable(VarName("Natural/blahblah"), BigInt(0)),
    "Natural/show123" -> Expression.Variable(VarName("Natural/show123"), BigInt(0)),
    "abc" -> Expression.Variable(VarName("abc"), BigInt(0)),
    "a-b/c" -> Expression.Variable(VarName("a-b/c"), BigInt(0)),
    "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
    "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
    "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
    "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
  )

  val primitiveExpressions: Seq[(String, Expression)] = Seq(
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
  )

  val selectorExpressions: Seq[(String, Expression)] = Seq(
      "x . y" -> Expression.Field(Expression.Variable(VarName("x"), BigInt(0)), FieldName("y"))
  )
}
