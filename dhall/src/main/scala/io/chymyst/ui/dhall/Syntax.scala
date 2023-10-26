package io.chymyst.ui.dhall

import enumeratum._
import io.chymyst.ui.dhall.CBORmodel.CBytes
import io.chymyst.ui.dhall.Grammar.hexStringToByteArray
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, VarName}

import java.time.LocalTime
import scala.util.chaining.scalaUtilChainingOps

object SyntaxConstants {
  final case class VarName(name: String) extends AnyVal

  final case class FieldName(name: String) extends AnyVal

  final case class ConstructorName(name: String) extends AnyVal

  trait HasCborCode[A, B] {
    def cborCode: B
  }

  trait HasCborCodeDict[B, A <: EnumEntry with HasCborCode[A, B]] {
    self: Enum[A] =>
    lazy val cborCodeDict: Map[B, A] = values.map { op => (op.cborCode, op) }.toMap
  }

  sealed abstract class Operator(val op: String, val cborCode: Int) extends EnumEntry with HasCborCode[Operator, Int]

  object Operator extends Enum[Operator] with HasCborCodeDict[Int, Operator] {
    val values = findValues

    case object Or extends Operator("||", 0)

    case object Plus extends Operator("+", 4)

    case object TextAppend extends Operator("++", 6)

    case object ListAppend extends Operator("#", 7)

    case object And extends Operator("&&", 1)

    case object CombineRecordTerms extends Operator("∧", 8)

    case object Prefer extends Operator("⫽", 9)

    case object CombineRecordTypes extends Operator("⩓", 10)

    case object Times extends Operator("*", 5)

    case object Equal extends Operator("==", 2)

    case object NotEqual extends Operator("!=", 3)

    case object Equivalent extends Operator("===", 12)

    case object Alternative extends Operator("?", 11)
  }

  sealed abstract class Builtin(override val entryName: String) extends EnumEntry

  object Builtin extends Enum[Builtin] {
    case object Bool extends Builtin("Bool")

    case object Bytes extends Builtin("Bytes")

    case object Date extends Builtin("Date")

    case object DateShow extends Builtin("Date/show")

    case object Double extends Builtin("Double")

    case object DoubleShow extends Builtin("Double/show")

    case object False extends Builtin("False")

    case object Integer extends Builtin("Integer")

    case object IntegerClamp extends Builtin("Integer/clamp")

    case object IntegerNegate extends Builtin("Integer/negate")

    case object IntegerShow extends Builtin("Integer/show")

    case object IntegerToDouble extends Builtin("Integer/toDouble")

    case object Kind extends Builtin("Kind")

    case object List extends Builtin("List")

    case object ListBuild extends Builtin("List/build")

    case object ListFold extends Builtin("List/fold")

    case object ListHead extends Builtin("List/head")

    case object ListIndexed extends Builtin("List/indexed")

    case object ListLast extends Builtin("List/last")

    case object ListLength extends Builtin("List/length")

    case object ListReverse extends Builtin("List/reverse")

    case object Natural extends Builtin("Natural")

    case object NaturalBuild extends Builtin("Natural/build")

    case object NaturalEven extends Builtin("Natural/even")

    case object NaturalFold extends Builtin("Natural/fold")

    case object NaturalIsZero extends Builtin("Natural/isZero")

    case object NaturalOdd extends Builtin("Natural/odd")

    case object NaturalShow extends Builtin("Natural/show")

    case object NaturalSubtract extends Builtin("Natural/subtract")

    case object NaturalToInteger extends Builtin("Natural/toInteger")

    case object None extends Builtin("None")

    case object Optional extends Builtin("Optional")

    case object Sort extends Builtin("Sort")

    case object Text extends Builtin("Text")

    case object TextReplace extends Builtin("Text/replace")

    case object TextShow extends Builtin("Text/show")

    case object Time extends Builtin("Time")

    case object TimeShow extends Builtin("Time/show")

    case object TimeZone extends Builtin("TimeZone")

    case object TimeZoneShow extends Builtin("TimeZone/show")

    case object True extends Builtin("True")

    case object Type extends Builtin("Type")

    override def values = findValues
  }

  // TODO: implement parsing into this type when appropriate, instead of parsing into Builtin. Also True, False.
  sealed trait Constant extends EnumEntry

  object Constant extends Enum[Constant] {
    val values = findValues

    case object Type extends Constant

    case object Kind extends Constant

    case object Sort extends Constant

    case object True extends Constant

    case object False extends Constant

  }

  sealed abstract class ImportMode(val cborCode: Int) extends EnumEntry with HasCborCode[ImportMode, Int]

  object ImportMode extends Enum[ImportMode] with HasCborCodeDict[Int, ImportMode] {
    val values = findValues

    case object Code extends ImportMode(0)

    case object RawBytes extends ImportMode(3)

    case object RawText extends ImportMode(1)

    case object Location extends ImportMode(2)
  }

  sealed abstract class Scheme(val cborCode: Int) extends EnumEntry with HasCborCode[Scheme, Int]

  object Scheme extends Enum[Scheme] with HasCborCodeDict[Int, Scheme] {
    val values = findValues

    case object HTTP extends Scheme(0)

    case object HTTPS extends Scheme(1)
  }

  sealed abstract class FilePrefix(val cborCode: Int) extends EnumEntry with HasCborCode[FilePrefix, Int]

  object FilePrefix extends Enum[FilePrefix] with HasCborCodeDict[Int, FilePrefix] {
    val values = findValues

    case object Absolute extends FilePrefix(2)

    case object Here extends FilePrefix(3) // ./something relative to the current working directory

    case object Parent extends FilePrefix(4) // ./something relative to the parent working directory

    case object Home extends FilePrefix(5) // ~/something relative to the user's home directory
  }

  sealed abstract class ImportType

  object ImportType {
    final case object Missing extends ImportType

    final case class Remote(url: URL, headers: Option[Expression]) extends ImportType

    final case class Path(filePrefix: FilePrefix, file: File) extends ImportType

    final case class Env(envVarName: String) extends ImportType
  }

  // The authority of http://user@host:port/foo is stored as "user@host:port".
  // The query of ?foo=1&bar=true is stored as "foo=1&bar=true".
  final case class URL(scheme: Scheme, authority: String, path: File, query: Option[String])

  final case class File(segments: Seq[String]) {
    require(segments.nonEmpty)
  }

  object File {
    def of(segments: Seq[String]) = if (segments.isEmpty) File(Seq("")) else File(segments)
  }
}

object Syntax {

  final case class DhallFile(shebangs: Seq[String], value: Expression)

  type Natural = BigInt

  type Integer = BigInt

  sealed trait Expression

  object Expression {
    final case class Variable(name: VarName, index: Natural) extends Expression

    final case class Lambda(name: VarName, tipe: Expression, body: Expression) extends Expression

    final case class Forall(name: VarName, tipe: Expression, body: Expression) extends Expression

    final case class Let(name: VarName, tipe: Option[Expression], subst: Expression, body: Expression) extends Expression

    final case class If(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression

    final case class Merge(record: Expression, update: Expression, tipe: Option[Expression]) extends Expression

    final case class ToMap(data: Expression, tipe: Option[Expression]) extends Expression

    final case class EmptyList(tipe: Expression) extends Expression

    final case class NonEmptyList(head: Expression, tail: Seq[Expression]) extends Expression

    final case class Annotation(data: Expression, tipe: Expression) extends Expression

    final case class Operator(lop: Expression, op: SyntaxConstants.Operator, rop: Expression) extends Expression

    final case class Application(func: Expression, arg: Expression) extends Expression

    final case class Field(base: Expression, name: FieldName) extends Expression

    final case class ProjectByLabels(base: Expression, labels: Seq[FieldName]) extends Expression

    final case class ProjectByType(base: Expression, by: Expression) extends Expression

    //    an expression of the form T::r is syntactic sugar for (T.default // r) : T.Type
    final case class Completion(base: Expression, target: Expression) extends Expression

    final case class Assert(assertion: Expression) extends Expression

    final case class With(data: Expression, pathComponents: Seq[PathComponent], body: Expression) extends Expression

    final case class DoubleLiteral(value: Double) extends Expression {
      override def equals(other: Any): Boolean = other.isInstanceOf[DoubleLiteral] && {
        val otherValue = other.asInstanceOf[DoubleLiteral].value
        (value == otherValue) || (value.isNaN && otherValue.isNaN)
      }
    }

    final case class NaturalLiteral(value: Natural) extends Expression

    object NaturalLiteral {
      def apply(value: Int): NaturalLiteral = {
        require(value >= 0)
        NaturalLiteral(BigInt(value))
      }
    }

    final case class IntegerLiteral(value: Integer) extends Expression

    object IntegerLiteral {
      def apply(value: Int): IntegerLiteral = {
        IntegerLiteral(BigInt(value))
      }
    }

    final case class TextLiteralNoInterp(value: String) extends Expression

    object TextLiteral {
      def ofString(s: String) = ofText(TextLiteralNoInterp(s))

      def ofText(textLiteralNoInterp: TextLiteralNoInterp) = TextLiteral(List(), textLiteralNoInterp.value)

      def empty = TextLiteral(List(), "")

      def ofExpression(expression: Expression) = TextLiteral(interpolations = List(("", expression)), trailing = "")
    }

    final case class TextLiteral(interpolations: List[(String, Expression)], trailing: String) extends Expression {
      /*
      instance Semigroup TextLiteral where
          Chunks xys₀ z₀ <> Chunks [] z₁ =
              Chunks xys₀ (z₀ <> z₁)
          Chunks xys₀ z₀ <> Chunks ((x₁, y₁) : xys₁) z₁ =
              Chunks (xys₀ <> ((z₀ <> x₁, y₁) : xys₁)) z₁
       */
      def ++(other: TextLiteral): TextLiteral = other.interpolations match {
        case List() =>
          TextLiteral(interpolations, trailing ++ other.trailing)
        case (headText, headExpr) :: tail =>
          TextLiteral(interpolations ++ ((trailing ++ headText, headExpr) :: tail), other.trailing)
      }

      // TODO: implement alignment by largest common indentation prefix of " " or "\t", see https://github.com/dhall-lang/dhall-lang/blob/master/standard/multiline.md
      private lazy val whitespacePrefixRegex = "[ \t]*".r

      lazy val whitespacePrefix: String = {
        val firstString = interpolations.headOption.map(_._1).getOrElse(trailing)
        whitespacePrefixRegex.findPrefixMatchOf(firstString).map(_.matched).getOrElse("")
      }

      def isEmpty: Boolean = trailing.isEmpty && interpolations.isEmpty

      private lazy val lines: Seq[TextLiteral] = {
        def loop(currentLine: TextLiteral, nextLine: TextLiteral): Seq[TextLiteral] = {
          nextLine.interpolations.headOption match {
            case None =>
              val splitLines = splitByAllNewlines(nextLine.trailing).map(TextLiteral.ofString)
              (currentLine ++ splitLines.head) +: splitLines.tail

            case Some((head, interpolation)) =>
              val headSplit = splitByAllNewlines(head)
              val l0 = headSplit.head // Guaranteed to exist.
              val ls = headSplit.tail
              if (ls.isEmpty) loop(
                currentLine ++ TextLiteral(List((head, interpolation)), ""),
                TextLiteral(nextLine.interpolations.tail, trailing),
              )
              else (currentLine ++ TextLiteral.ofString(l0)) +: (ls.init.map(TextLiteral.ofString) ++ loop(
                TextLiteral(List((ls.last, interpolation)), ""),
                TextLiteral(nextLine.interpolations.tail, trailing),
              ))
          }
        }

        loop(TextLiteral.empty, this)
      }

      def align: TextLiteral = {
        def longestCommonPrefix(a: String, b: String): String = a.iterator.zip(b.iterator).takeWhile { case (x, y) => x == y }.map(_._1).mkString

        val removeEmpty: Seq[TextLiteral] = lines.init.filterNot(_.isEmpty) :+ lines.last
        val longestCommonIndent: String = removeEmpty.map(_.whitespacePrefix).reduceRight(longestCommonPrefix)
        removeIndentsAndConcatenate(longestCommonIndent.length)
      }

      private def splitByAllNewlines(s: String): Seq[String] =
        s.split("\r\n", -1)
          .flatMap(_.split("\n", -1))
          .toSeq
          .pipe(s => if (s.isEmpty) Seq("") else s)

      private def removeIndentsAndConcatenate(indent: Int): TextLiteral = {
        def join(a: TextLiteral, b: TextLiteral): TextLiteral = a ++ TextLiteral.ofString("\n") ++ b

        def joinLines(lines: Seq[TextLiteral]): TextLiteral = lines.reduceRight(join)

        joinLines(lines.map(_.stripPrefix(indent))).escape
      }

      def stripPrefix(indent: Int): TextLiteral = interpolations.headOption match {
        case Some((head, tail)) => copy(interpolations = (head.drop(indent), tail) +: interpolations.tail)
        case None => copy(trailing = trailing.drop(indent))
      }

      def mapStrings(f: String => String): TextLiteral = copy(
        interpolations = interpolations.map { case (head, tail) => (f(head), tail) },
        trailing = f(trailing),
      )

      private def reEscape(s: String): String = s.replace("'''", "''").replace("''${", "${")

      private def escape: TextLiteral = mapStrings(reEscape)

    }

    // The hex string must be lowercase.
    final case class BytesLiteral private(hex: String) extends Expression {
      val bytes: Array[Byte] = hexStringToByteArray(hex)
    }

    object BytesLiteral {
      def of(hex: String) = BytesLiteral(hex.toUpperCase)

      def of(bytes: Array[Byte]) = BytesLiteral(CBytes.byteArrayToHexString(bytes))
    }

    final case class DateLiteral(year: Int, month: Int, day: Int) extends Expression

    final case class TimeLiteral(time: LocalTime) extends Expression

    final case class TimeZoneLiteral(totalMinutes: Int) extends Expression

    final case class RecordType(defs: Seq[(FieldName, Expression)]) extends Expression {
      def sorted = RecordType(defs.sortBy(_._1.name))
    }

    final case class RecordLiteral(defs: Seq[(FieldName, Expression)]) extends Expression {
      def sorted = RecordLiteral(defs.sortBy(_._1.name))
    }

    object RecordLiteral {
      // Parse a non-empty sequence of RawRecordLiteral's into a RecordLiteral.
      def of(values: Seq[RawRecordLiteral]): RecordLiteral = {
        /* See https://github.com/dhall-lang/dhall-lang/blob/master/standard/README.md#record-syntactic-sugar

          ... a record literal of the form:

          { x.y = 1, x.z = 1 }

          ... first desugars dotted fields to nested records:

          { x = { y = 1 }, x = { z = 1 } }

          ... and then desugars duplicate fields by merging them using ∧:

          { x = { y = 1 } ∧ { z = 1} }

          ... this conversion occurs at parse-time ...

          See https://github.com/dhall-lang/dhall-lang/blob/master/standard/record.md

           */
        val desugared: Seq[(FieldName, Expression)] = values.map {
          // Desugar { x } into { x = x }.
          case RawRecordLiteral(base, None) => (base, Expression.Variable(VarName(base.name), BigInt(0)))

          // Desugar { w.x.y.z = expr } into {w = {x = { y = {z = expr }}}}.
          case RawRecordLiteral(base, Some((fields, target))) => (base, fields.foldRight(target) { (field, expr) => RecordLiteral(Seq((field, expr))) })
        }

        // Desugar repeated field names { x = { y = 1 }, x = { z = 1 } } into { x = { y = 1 } ∧ { z = 1} }. This is needed at the top nested level only.
        def desugarRepetition(defs: Seq[(FieldName, Expression)]): Seq[(FieldName, Expression)] = {
          val recordMap: Map[FieldName, Expression] =
            defs.groupBy(_._1)
              .map { case (field, subDefs) =>
                (field, subDefs.map(_._2).reduce((a, b) => Expression.Operator(a, SyntaxConstants.Operator.CombineRecordTerms, b)))
              }
          // Preserve the original order of definitions.
          defs.map(_._1).distinct.map { fieldName => (fieldName, recordMap(fieldName)) }
        }

        RecordLiteral(desugarRepetition(desugared))
      }
    }

    // Raw record syntax: { x.y.z = 1 } that needs to be processed further.
    final case class RawRecordLiteral(base: FieldName, defs: Option[(Seq[FieldName], Expression)]) extends Expression

    final case class UnionType(defs: Seq[(ConstructorName, Option[Expression])]) extends Expression {
      def sorted = UnionType(defs.sortBy(_._1.name))
    }

    final case class ShowConstructor(data: Expression) extends Expression

    final case class Import(importType: SyntaxConstants.ImportType, importMode: SyntaxConstants.ImportMode, digest: Option[BytesLiteral]) extends Expression

    final case class KeywordSome(data: Expression) extends Expression

    final case class Builtin(builtin: SyntaxConstants.Builtin) extends Expression

    final case class Constant(constant: SyntaxConstants.Constant) extends Expression

  }

  sealed trait PathComponent

  object PathComponent {
    final case class Label(fieldName: FieldName) extends PathComponent

    final case object DescendOptional extends PathComponent
  }

}

trait Monoid[A] {
  def empty: A

  def combine(x: A, y: A): A
}