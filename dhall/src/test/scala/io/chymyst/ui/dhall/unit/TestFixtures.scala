package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.SyntaxConstants._
import io.chymyst.ui.dhall.{Grammar, SyntaxConstants}
import io.chymyst.ui.dhall.unit.TestUtils.v

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
    "Natural-blahblah" -> v("Natural-blahblah"),
    "Natural/blahblah" -> v("Natural/blahblah"),
    "Natural/show123" -> v("Natural/show123"),
    "abc" -> v("abc"),
    "a-b/c" -> v("a-b/c"),
    "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
    "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
    "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
    "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
  )

  val identifiersWithBackquote = Seq(
    "`abc`" -> v("abc"),
    "` `" -> v(" "),
    "`0%!#${}%^`" -> v("0%!#${}%^"),
  ) ++ (Grammar.simpleKeywords ++ Grammar.builtinSymbolNames).sorted.map { name => s"`$name`" -> v(name) }

  val primitiveExpressions: Seq[(String, Expression)] = Seq(
    "12345" -> Expression.NaturalLiteral(BigInt(12345)),
    "-4312.2" -> Expression.DoubleLiteral(-4312.2),
    "\"123\"" -> Expression.TextLiteral.ofText(Expression.TextLiteralNoInterp("123")),
    """''
      |line
      |''""".stripMargin -> Expression.TextLiteral.ofText(Expression.TextLiteralNoInterp("line\n")),
    "x" -> v("x"),
    "(x)" -> v("x"),
    "( x )" -> v("x"),
    "( -12345  )" -> Expression.IntegerLiteral(BigInt(-12345)),
    "a-b/c" -> v("a-b/c"),
    "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Expression.Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
    "[1,2,3]" -> Expression.NonEmptyList(Expression.NaturalLiteral(BigInt(1)), Seq(Expression.NaturalLiteral(BigInt(2)), Expression.NaturalLiteral(BigInt(3)))),
    "Kind" -> Expression.Builtin(SyntaxConstants.Builtin.Kind),
    "Natural/show" -> Expression.Builtin(SyntaxConstants.Builtin.NaturalShow),
    "Natural" -> Expression.Builtin(SyntaxConstants.Builtin.Natural),
    "{foo: Natural, bar: Type}" -> Expression.RecordType(Seq(
      (FieldName("bar"), Expression.Builtin(SyntaxConstants.Builtin.Type)),
      (FieldName("foo"), Expression.Builtin(SyntaxConstants.Builtin.Natural)),
    )),
    "{ foo = 1, bar = 2 }" -> Expression.RecordLiteral(Seq(
      (FieldName("bar"), Expression.NaturalLiteral(2)),
      (FieldName("foo"), Expression.NaturalLiteral(1)),
    )),
    "< Foo : Integer | Bar : Bool >" -> Expression.UnionType(Seq(
      (ConstructorName("Foo"), Some(Expression.Builtin(SyntaxConstants.Builtin.Integer))),
      (ConstructorName("Bar"), Some(Expression.Builtin(SyntaxConstants.Builtin.Bool)))),
    ).sorted,
    "< Foo | Bar : Bool >" -> Expression.UnionType(List((ConstructorName("Foo"), None), (ConstructorName("Bar"), Some(Expression.Builtin(SyntaxConstants.Builtin.Bool))))).sorted,
  )

  val selectorExpressions = Map(
    "x.y" -> Expression.Field(v("x"), FieldName("y")),
    "x . y . z" -> Expression.Field(Expression.Field(v("x"), FieldName("y")), FieldName("z")),
    "x .y . (Natural)" -> Expression.ProjectByType(Expression.Field(v("x"), FieldName("y")), Expression.Builtin(SyntaxConstants.Builtin.Natural)),
    "x. {y,z }" -> Expression.ProjectByLabels(v("x"), Seq(FieldName("y"), FieldName("z"))),
  )

  val sha256example = "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF"
  val sha256lc = "16173e984d35ee3ffd8b6b79167df89480e67d1cd03ea5d0fc93689e4d928e61"

  val completionExpressions = Seq(
    "x .y .( Natural) ::x .{y ,z}" -> Expression.Completion(selectorExpressions("x .y . (Natural)"),
      selectorExpressions("x. {y,z }")),
  )

  val importExpressions: Seq[(String, Expression)] = Seq(
    s"./a.dhall sha256:$sha256example" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("a.dhall"))), ImportMode.Code, Some(Expression.BytesLiteral(sha256example))),
    s"./a.dhall sha256:$sha256lc" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("a.dhall"))), ImportMode.Code, Some(Expression.BytesLiteral(sha256lc.toUpperCase))),
    "./local/import as Location" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Location, None),
    s"./local/import sha256:$sha256example" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Code, Some
    (Expression.BytesLiteral(sha256example))),
    s"./local/import.dhall sha256:$sha256example as Text" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import.dhall"))), ImportMode.RawText,
      Some(Expression.BytesLiteral(sha256example))),
    s"./local/import sha256:$sha256example as Bytes" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode
      .RawBytes, Some(Expression.BytesLiteral(sha256example))),
    "env:HOME as Text" -> Expression.Import(ImportType.Env("HOME"), ImportMode.RawText, None),
    s"https://example.com/a/b?c=d using headers123 sha256:$sha256example as Bytes" -> Expression.Import(ImportType.Remote(URL(Scheme.HTTPS, "example.com", File
    (Seq("a", "b")), Some("c=d")), Some(v("headers123"))), ImportMode.RawBytes, Some(Expression.BytesLiteral(sha256example))),
  )

  val plusExpressions: Seq[(String, Expression)] = Seq(
    "1 + 1" -> Expression.Operator(Expression.NaturalLiteral(1), SyntaxConstants.Operator.Plus, Expression.NaturalLiteral(1)),
    "10 + +10" -> Expression.Operator(Expression.NaturalLiteral(10), SyntaxConstants.Operator.Plus, Expression.IntegerLiteral(10)),
    "10 + -10" -> Expression.Operator(Expression.NaturalLiteral(10), SyntaxConstants.Operator.Plus, Expression.IntegerLiteral(-10)),
    "10 +10" -> Expression.Application(Expression.NaturalLiteral(10), Expression.IntegerLiteral(10)),
    "10 ++10" -> Expression.Operator(Expression.NaturalLiteral(10), SyntaxConstants.Operator.TextAppend, Expression.NaturalLiteral(10)),
    "1.0 + 2.0" -> Expression.Operator(Expression.DoubleLiteral(1.0), SyntaxConstants.Operator.Plus, Expression.DoubleLiteral(2.0)),
    "1.0 -2.0" -> Expression.Application(Expression.DoubleLiteral(1.0), Expression.DoubleLiteral(-2.0)),
    "1 ++ [1,2,3]" -> Expression.Operator(Expression.NaturalLiteral(1), SyntaxConstants.Operator.TextAppend, primitiveExpressions.toMap.apply("[1,2,3]")),
  )

  val recordExpressions: Seq[(String, Expression)] = Seq(
    "{ foo, bar }" -> Expression.RecordLiteral(List(
      (FieldName("bar"), v("bar")),
      (FieldName("foo"), v("foo")),
    )),
  )

  // Note: a `let_binding` must end with a whitespace.
  val letBindings: Seq[(String, (VarName, Option[Expression], Expression))] = Seq(
    "let x = 1 " -> (VarName("x"), None, Expression.NaturalLiteral(1)),
    "let x : Integer = 1 " -> (VarName("x"), Some(Expression.Builtin(SyntaxConstants.Builtin.Integer)), Expression.NaturalLiteral(1)),
  )

  val letBindingExpressions: Seq[(String, Expression)] = Seq(
    "let x = 1 in y" -> Expression.Let(VarName("x"), None, Expression.NaturalLiteral(1), v("y")),
    "let x = 1 let y = 2 in z" -> Expression.Let(VarName("x"), None, Expression.NaturalLiteral(1), Expression.Let(VarName("y"), None, Expression.NaturalLiteral(2), v("z"))),
    "let x = 1 in let y = 2 in z" -> Expression.Let(VarName("x"), None, Expression.NaturalLiteral(1), Expression.Let(VarName("y"), None, Expression.NaturalLiteral(2), v("z"))),
    "let `in` = 1 in `let`" -> Expression.Let(VarName("in"), None, Expression.NaturalLiteral(1), v("let")),
  )

  val interpolationExpressions: Seq[(String, Expression)] =
    Seq(
      "${x}" -> v("x"),
      "${1}" -> Expression.NaturalLiteral(1),
      "${\"x\"}" -> Expression.TextLiteral(List(), "x"),
    )

  val doubleQuotedExpressions: Seq[(String, Expression)] =
    Seq(
      "\"\"" -> Expression.TextLiteral(List(
      ), ""),
      "\"x\"" -> Expression.TextLiteral(List(
      ), "x"),
      "\"${x}\"" -> Expression.TextLiteral(List(
        ("", v("x")),
      ), ""),
      "\"a${x}\"" -> Expression.TextLiteral(List(
        ("a", v("x")),
      ), ""),
      "\"${x}b\"" -> Expression.TextLiteral(List(
        ("", v("x")),
      ), "b"),
      "\"a${x}b\"" -> Expression.TextLiteral(List(
        ("a", v("x")),
      ), "b"),
      "\"${x}a${y}b\"" -> Expression.TextLiteral(List(
        ("", v("x")),
        ("a", v("y")),
      ), "b"),
    )

  val singleQuotedExpressions: Seq[(String, Expression)] = doubleQuotedExpressions.map { case (s, expr) => (s.replaceFirst("\"",
    "''\n").replace("\"", "''"), expr)
  }
}
