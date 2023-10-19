package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Grammar.hexStringToByteArray
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.Syntax.Expression.TextLiteral
import io.chymyst.ui.dhall.SyntaxConstants
import io.chymyst.ui.dhall.SyntaxConstants.{FieldName, File, FilePrefix, ImportMode, ImportType, VarName}

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

  val selectorExpressions = Map(
    "x.y" -> Expression.Field(Expression.Variable(VarName("x"), BigInt(0)), FieldName("y")),
    "x . y . z" -> Expression.Field(Expression.Field(Expression.Variable(VarName("x"), BigInt(0)), FieldName("y")), FieldName("z")),
    "x .y . (Natural)" -> Expression.ProjectByType(Expression.Field(Expression.Variable(VarName("x"), BigInt(0)), FieldName("y")), Expression.Builtin(SyntaxConstants.Builtin.Natural)),
    "x. {y,z }" -> Expression.ProjectByLabels(Expression.Variable(VarName("x"), BigInt(0)), Seq(FieldName("y"), FieldName("z"))),
  )

  val sha256example = "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF"

  val completionExpressions = Seq(
    "x .y .( Natural) ::x .{y ,z}" -> Expression.Completion(selectorExpressions("x .y . (Natural)"),
      selectorExpressions("x. {y,z }")),
  )

  val importExpressions: Seq[(String, Expression)] = Seq(
    "./local/import as Location" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Location, None),
    s"./local/import sha256:$sha256example" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Code, Some
    (hexStringToByteArray(sha256example))),
    s"./local/import sha256:$sha256example as Text" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.RawText, Some(hexStringToByteArray(sha256example))),
    s"./local/import sha256:$sha256example as Bytes" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.RawBytes, Some(hexStringToByteArray(sha256example))),
  )
}
