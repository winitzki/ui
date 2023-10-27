package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Grammar.TextLiteralNoInterp
import io.chymyst.ui.dhall.Syntax.{Expression, ExpressionScheme}
import io.chymyst.ui.dhall.Syntax.ExpressionScheme._
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

  val identifiers: Seq[(String, Expression)] = Seq(
    "Natural-blahblah" -> v("Natural-blahblah"),
    "Natural/blahblah" -> v("Natural/blahblah"),
    "Natural/show123" -> v("Natural/show123"),
    "abc" -> v("abc"),
    "a-b/c" -> v("a-b/c"),
    "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
    "Kind" -> ExprBuiltin(SyntaxConstants.Builtin.Kind),
    "Natural/show" -> ExprBuiltin(SyntaxConstants.Builtin.NaturalShow),
    "Natural" -> ExprBuiltin(SyntaxConstants.Builtin.Natural),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val identifiersWithBackquote: Seq[(String, Expression)] = (Seq(
    "`abc`" -> v("abc"),
    "` `" -> v(" "),
    "`0%!#${}%^`" -> v("0%!#${}%^"),
  ) ++ (Grammar.simpleKeywords ++ Grammar.builtinSymbolNames).sorted.map { name => s"`$name`" -> v(name) }
    ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val primitiveExpressions: Seq[(String, Expression)] = Seq(
    "12345" -> NaturalLiteral(BigInt(12345)),
    "-4312.2" -> DoubleLiteral(-4312.2),
    "\"123\"" -> TextLiteral.ofText[Expression](TextLiteralNoInterp("123")),
    """''
      |line
      |''""".stripMargin -> TextLiteral.ofText[Expression](TextLiteralNoInterp("line\n")),
    "x" -> v("x"),
    "(x)" -> v("x"),
    "( x )" -> v("x"),
    "( -12345  )" -> IntegerLiteral(BigInt(-12345)),
    "a-b/c" -> v("a-b/c"),
    "_xyz       @   \t\t\t\n\n           123451234512345123451234512345" -> Variable(VarName("_xyz"), BigInt("123451234512345123451234512345")),
    "[1,2,3]" -> NonEmptyList(NaturalLiteral(BigInt(1)), Seq(NaturalLiteral(BigInt(2)), NaturalLiteral(BigInt(3)))),
    "Kind" -> ExprBuiltin(SyntaxConstants.Builtin.Kind),
    "Natural/show" -> ExprBuiltin(SyntaxConstants.Builtin.NaturalShow),
    "Natural" -> ExprBuiltin(SyntaxConstants.Builtin.Natural),
    "{foo: Natural, bar: Type}" -> RecordType(Seq(
      (FieldName("bar"), ExprBuiltin(SyntaxConstants.Builtin.Type)),
      (FieldName("foo"), ExprBuiltin(SyntaxConstants.Builtin.Natural)),
    )),
    "{ foo = 1, bar = 2 }" -> RecordLiteral(Seq(
      (FieldName("bar"), NaturalLiteral(2)),
      (FieldName("foo"), NaturalLiteral(1)),
    )),
    "< Foo : Integer | Bar : Bool >" -> UnionType(Seq(
      (ConstructorName("Foo"), Some(ExprBuiltin(SyntaxConstants.Builtin.Integer))),
      (ConstructorName("Bar"), Some(ExprBuiltin(SyntaxConstants.Builtin.Bool)))),
    ).sorted,
    "< Foo | Bar : Bool >" -> UnionType(List((ConstructorName("Foo"), None), (ConstructorName("Bar"), Some(ExprBuiltin(SyntaxConstants.Builtin.Bool))))).sorted,
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val selectorExpressions: Map[String, Expression] = Map(
    "x.y" -> Field(v("x"), FieldName("y")),
    "x . y . z" -> Field(Field(v("x"), FieldName("y")), FieldName("z")),
    "x .y . (Natural)" -> ProjectByType(Field(v("x"), FieldName("y")), ExprBuiltin(SyntaxConstants.Builtin.Natural)),
    "x. {y,z }" -> ProjectByLabels(v("x"), Seq(FieldName("y"), FieldName("z"))),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val sha256example = "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF"
  val sha256lc = "16173e984d35ee3ffd8b6b79167df89480e67d1cd03ea5d0fc93689e4d928e61"

  val completionExpressions = Seq(
    "x .y .( Natural) ::x .{y ,z}" -> Completion(selectorExpressions("x .y . (Natural)"),
      selectorExpressions("x. {y,z }")),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val importExpressions: Seq[(String, Expression)] = Seq(
    s"./a.dhall sha256:$sha256example" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("a.dhall"))), ImportMode.Code, Some(BytesLiteral(sha256example))),
    s"./a.dhall sha256:$sha256lc" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("a.dhall"))), ImportMode.Code, Some(BytesLiteral(sha256lc.toUpperCase))),
    "./local/import as Location" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Location, None),
    s"./local/import sha256:$sha256example" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.Code, Some
    (BytesLiteral(sha256example))),
    s"./local/import.dhall sha256:$sha256example as Text" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("local", "import.dhall"))), ImportMode.RawText,
      Some(BytesLiteral(sha256example))),
    s"./local/import sha256:$sha256example as Bytes" -> Import[Expression](ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode
      .RawBytes, Some(BytesLiteral(sha256example))),
    "env:HOME as Text" -> Import[Expression](ImportType.Env("HOME"), ImportMode.RawText, None),
    s"https://example.com/a/b?c=d using headers123 sha256:$sha256example as Bytes" -> Import[Expression](ImportType.Remote(URL(Scheme.HTTPS, "example.com", File
    (Seq("a", "b")), Some("c=d")), Some(v("headers123"))), ImportMode.RawBytes, Some(BytesLiteral(sha256example))),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val plusExpressions: Seq[(String, Expression)] = Seq(
    "1 + 1" -> ExprOperator(NaturalLiteral(1), SyntaxConstants.Operator.Plus, NaturalLiteral(1)),
    "10 + +10" -> ExprOperator(NaturalLiteral(10), SyntaxConstants.Operator.Plus, IntegerLiteral(10)),
    "10 + -10" -> ExprOperator(NaturalLiteral(10), SyntaxConstants.Operator.Plus, IntegerLiteral(-10)),
    "10 +10" -> Application(NaturalLiteral(10), IntegerLiteral(10)),
    "10 ++10" -> ExprOperator(NaturalLiteral(10), SyntaxConstants.Operator.TextAppend, NaturalLiteral(10)),
    "1.0 + 2.0" -> ExprOperator(DoubleLiteral(1.0), SyntaxConstants.Operator.Plus, DoubleLiteral(2.0)),
    "1.0 -2.0" -> Application(DoubleLiteral(1.0), DoubleLiteral(-2.0)),
    "1 ++ [1,2,3]" -> ExprOperator(NaturalLiteral(1), SyntaxConstants.Operator.TextAppend, primitiveExpressions.toMap.apply("[1,2,3]")),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val recordExpressions: Seq[(String, Expression)] = Seq(
    "{ foo, bar }" -> RecordLiteral[Expression](List(
      (FieldName("bar"), v("bar")),
      (FieldName("foo"), v("foo")),
    )),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  // Note: a `let_binding` must end with a whitespace.
  val letBindings: Seq[(String, (VarName, Option[Expression], Expression))] = Seq(
    "let x = 1 " -> (VarName("x"), None, NaturalLiteral(1)),
    "let x : Integer = 1 " -> (VarName("x"), Some(ExprBuiltin(SyntaxConstants.Builtin.Integer)), NaturalLiteral(1)),
  ).map { case (k, (v, p, n)) => (k, (v, p.map(Expression.apply), Expression(n))) }

  val letBindingExpressions: Seq[(String, Expression)] = Seq(
    "let x = 1 in y" -> Let(VarName("x"), None, NaturalLiteral(1), v("y")),
    "let x = 1 let y = 2 in z" -> Let(VarName("x"), None, NaturalLiteral(1), Let(VarName("y"), None, NaturalLiteral(2), v("z"))),
    "let x = 1 in let y = 2 in z" -> Let(VarName("x"), None, NaturalLiteral(1), Let(VarName("y"), None, NaturalLiteral(2), v("z"))),
    "let `in` = 1 in `let`" -> Let(VarName("in"), None, NaturalLiteral(1), v("let")),
  ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val interpolationExpressions: Seq[(String, Expression)] =
    Seq(
      "${x}" -> v("x"),
      "${1}" -> NaturalLiteral(1),
      "${\"x\"}" -> TextLiteral(List(), "x"),
    ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val doubleQuotedExpressions: Seq[(String, Expression)] =
    Seq(
      "\"\"" -> TextLiteral(List(
      ), ""),
      "\"x\"" -> TextLiteral(List(
      ), "x"),
      "\"${x}\"" -> TextLiteral(List(
        ("", v("x")),
      ), ""),
      "\"a${x}\"" -> TextLiteral(List(
        ("a", v("x")),
      ), ""),
      "\"${x}b\"" -> TextLiteral(List(
        ("", v("x")),
      ), "b"),
      "\"a${x}b\"" -> TextLiteral(List(
        ("a", v("x")),
      ), "b"),
      "\"${x}a${y}b\"" -> TextLiteral(List(
        ("", v("x")),
        ("a", v("y")),
      ), "b"),
    ).map { case (k, v) => (k, Expression(v.asInstanceOf[ExpressionScheme[Expression]])) }

  val singleQuotedExpressions: Seq[(String, Expression)] = doubleQuotedExpressions.map { case (s, expr) => (s.replaceFirst("\"",
    "''\n").replace("\"", "''"), expr)
  }
}
