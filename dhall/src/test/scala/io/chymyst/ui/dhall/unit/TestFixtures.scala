package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import fastparse._
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.Syntax.Expression.{Builtin, BytesLiteral, NaturalLiteral, TextLiteral, Variable}
import io.chymyst.ui.dhall.SyntaxConstants
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, File, FilePrefix, ImportMode, ImportType, Scheme, URL, VarName}

import java.io.{PrintWriter, StringWriter}
import scala.util.Try

object TestFixtures {
  def check[A](grammarRule: P[_] => P[A], input: String, expectedResult: A, lastIndex: Int): Unit = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Parsing input '$input', got Success($value, $index), expecting Success($expectedResult, $lastIndex)")
      case Parsed.Failure(message, index, extra) =>
        println(s"Error: Parsing input '$input', expected Success($expectedResult, $index) but got Failure('$message', $index, ${extra.stack})")
    }
    expect((input != null) && (parsed == Parsed.Success(expectedResult, lastIndex)))
  }

  def toFail[A](grammarRule: P[_] => P[A], input: String, parsedInput: String, expectedMessage: String, lastIndex: Int): Unit = {
    val parsed = parse(input, grammarRule)
    parsed match {
      case Parsed.Success(value, index) =>
        println(s"Error: Parsing input '$input', expected Failure but got Success($value, $index)")
        expect(parsed == Parsed.Failure(parsedInput, lastIndex, null))
      case f@Parsed.Failure(message, index, extra) =>
        println(s"Parsing input '$input', expected index $lastIndex, got Failure('$message', $index, ${extra.stack}), message '${f.msg}' as expected")
        expect(input != null && (f.msg contains expectedMessage), input != null && f.index == lastIndex)
    }
  }

  def check[A](successExamples: Seq[(String, A)], grammarRule: P[_] => P[A]): Unit = {
    val results = successExamples.map { case (s, d) =>
      Try(check(grammarRule(_), s, d, s.length))
    }
    if (results.forall(_.isSuccess))
      println(s"All ${successExamples.size} examples passed.")
    else {
      println(s"Error: ${results.count(_.isFailure)} examples failed:")
      val message = results.filter(_.isFailure).map(_.failed.get).map { t =>
        val stackTrace = new StringWriter
        t.printStackTrace(new PrintWriter(stackTrace))
        stackTrace.flush()
        t.getMessage + "\n\n" + stackTrace.toString
      }.mkString("\n")
      throw new Exception(message)
    }
  }

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
    "{foo: Natural, bar: Type}" -> Expression.RecordType(Seq(
      (FieldName("foo"), Expression.Builtin(SyntaxConstants.Builtin.Natural)),
      (FieldName("bar"), Expression.Builtin(SyntaxConstants.Builtin.Type)),
    )),
    "{ foo = 1, bar = 2 }" -> Expression.RecordLiteral(Seq(
      (FieldName("foo"), Expression.NaturalLiteral(1)),
      (FieldName("bar"), Expression.NaturalLiteral(2)),
    )),
    "< Foo : Integer | Bar : Bool >" -> Expression.UnionType(Seq(
      (ConstructorName("Foo"), Some(Expression.Builtin(SyntaxConstants.Builtin.Integer))),
      (ConstructorName("Bar"), Some(Builtin(SyntaxConstants.Builtin.Bool)))),
    ),
    "< Foo | Bar : Bool >" -> Expression.UnionType(List((ConstructorName("Foo"), None), (ConstructorName("Bar"), Some(Builtin(SyntaxConstants.Builtin.Bool))))),
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
    (BytesLiteral(sha256example))),
    s"./local/import sha256:$sha256example as Text" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.RawText, Some(BytesLiteral(sha256example))),
    s"./local/import sha256:$sha256example as Bytes" -> Expression.Import(ImportType.Path(FilePrefix.Here, File(Seq("local", "import"))), ImportMode.RawBytes, Some(BytesLiteral(sha256example))),
    "env:HOME as Text" -> Expression.Import(ImportType.Env("HOME"), ImportMode.RawText, None),
    s"https://example.com/a/b?c=d using headers123 sha256:$sha256example as Bytes" -> Expression.Import(ImportType.Remote(URL(Scheme.HTTPS, "example.com", File
    (Seq("a", "b")), Some("c=d")), Expression.Variable(VarName("headers123"), BigInt(0))), ImportMode.RawBytes, Some(BytesLiteral(sha256example))),
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
    "{ foo, bar }" -> Expression.RecordLiteral(List((FieldName("foo"), Variable(VarName("foo"), BigInt(0))), (FieldName("bar"), Variable(VarName("bar"), BigInt
    (0))
    ))),
  )

  // Note: a `let_binding` must end with a whitespace.
  val letBindings: Seq[(String, (VarName, Option[Expression], Expression))] = Seq(
    "let x = 1 " -> (VarName("x"), None, Expression.NaturalLiteral(1)),
    "let x : Integer = 1 " -> (VarName("x"), Some(Expression.Builtin(SyntaxConstants.Builtin.Integer)), Expression.NaturalLiteral(1)),
  )

  val letBindingExpressions: Seq[(String, Expression)] = Seq(
    "let x = 1 in y" -> Expression.Let(VarName("x"), None, NaturalLiteral(1), Variable(VarName("y"), 0)),
    "let x = 1 let y = 2 in z" -> Expression.Let(VarName("x"), None, NaturalLiteral(1), Expression.Let(VarName("y"), None, NaturalLiteral(2), Variable(VarName("z"), 0))),
    "let x = 1 in let y = 2 in z" -> Expression.Let(VarName("x"), None, NaturalLiteral(1), Expression.Let(VarName("y"), None, NaturalLiteral(2), Variable(VarName("z"), 0))),
  )
}
