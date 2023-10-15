package io.chymyst.ui.dhall

import enumeratum._
import io.chymyst.ui.dhall.Syntax.Expression

import java.time.{LocalDate, LocalTime, ZoneOffset}

object SyntaxConstants {
  sealed abstract class Operator(val op: String) extends EnumEntry

  object Operator extends Enum[Operator] {
    val values = findValues

    case object Or extends Operator("||")

    case object Plus extends Operator("+")

    case object TextAppend extends Operator("++")

    case object ListAppend extends Operator("#")

    case object And extends Operator("&&")

    case object CombineRecordTerms extends Operator("∧")

    case object Prefer extends Operator("⫽")

    case object CombineRecordTypes extends Operator("⩓")

    case object Times extends Operator("*")

    case object Equal extends Operator("==")

    case object NotEqual extends Operator("!=")

    case object Equivalent extends Operator("===")

    case object Alternative extends Operator("?")
  }

  sealed abstract class Builtin(val syntax: String)

  object Builtin {
    case object DateShow extends Builtin("Date/Show")

    case object DoubleShow extends Builtin("Double/Show")

    case object IntegerClamp extends Builtin("Integer/Clamp")

    case object IntegerNegate extends Builtin("Integer/Negate")

    case object IntegerShow extends Builtin("Integer/Show")

    case object IntegerToDouble extends Builtin("Integer/ToDouble")

    case object ListBuild extends Builtin("List/Build")

    case object ListFold extends Builtin("List/Fold")

    case object ListHead extends Builtin("List/Head")

    case object ListIndexed extends Builtin("List/Indexed")

    case object ListLast extends Builtin("List/Last")

    case object ListLength extends Builtin("List/Length")

    case object ListReverse extends Builtin("List/Reverse")

    case object NaturalBuild extends Builtin("Natural/Build")

    case object NaturalEven extends Builtin("Natural/Even")

    case object NaturalFold extends Builtin("Natural/Fold")

    case object NaturalIsZero extends Builtin("Natural/IsZero")

    case object NaturalOdd extends Builtin("Natural/Odd")

    case object NaturalShow extends Builtin("Natural/Show")

    case object NaturalSubtract extends Builtin("Natural/Subtract")

    case object NaturalToInteger extends Builtin("Natural/ToInteger")

    case object TextReplace extends Builtin("Text/Replace")

    case object TextShow extends Builtin("Text/Show")

    case object TimeShow extends Builtin("Time/Show")

    case object TimeZoneShow extends Builtin("TimeZone/Show")

    case object Bool extends Builtin("Bool")

    case object Bytes extends Builtin("Bytes")

    case object Date extends Builtin("Date")

    case object Double extends Builtin("Double")

    case object False extends Builtin("False")

    case object Integer extends Builtin("Integer")

    case object List extends Builtin("List")

    case object Natural extends Builtin("Natural")

    case object None extends Builtin("None")

    case object Optional extends Builtin("Optional")

    case object Text extends Builtin("Text")

    case object Time extends Builtin("Time")

    case object TimeZone extends Builtin("TimeZone")

    case object True extends Builtin("True")

  }

  sealed trait Constant extends EnumEntry

  object Constant extends Enum[Constant] {
    val values = findValues

    case object Type extends Constant

    case object Kind extends Constant

    case object Sort extends Constant
  }

  sealed trait ImportMode extends EnumEntry

  object ImportMode extends Enum[ImportMode] {
    val values = findValues

    case object Code extends ImportMode

    case object RawBytes extends ImportMode

    case object RawText extends ImportMode

    case object Location extends ImportMode
  }

  sealed trait Scheme

  object Scheme {
    case object HTTP extends Scheme

    case object HTTPS extends Scheme
  }

  sealed trait FilePrefix

  object FilePrefix {
    case object Absolute extends FilePrefix

    case object Here extends FilePrefix // ./something relative to the current working directory

    case object Parent extends FilePrefix // ./something relative to the parent working directory

    case object Home extends FilePrefix // ~/something relative to the user's home directory
  }

  sealed trait ImportType

  object ImportType {
    final case object Missing extends ImportType

    final case class Remote(url: URL, headers: Expression) extends ImportType

    final case class Path(filePrefix: FilePrefix, file: File) extends ImportType

    final case class Env(envVarName: String) extends ImportType
  }

  final case class URL(scheme: Scheme, authority: String, path: File, query: Option[String])

  final case class File(directoryPath: List[String], name: String)

}

object Syntax {
  // TODO: use Natural from spires if needed
  type Natural = Int

  sealed trait Expression

  object Expression {
    final case class Variable(name: String, index: Int) extends Expression

    final case class Lambda(name: String, tipe: Expression, body: Expression) extends Expression

    final case class Forall(name: String, tipe: Expression, body: Expression) extends Expression

    final case class Let(name: String, tipe: Option[Expression], subst: Expression, body: Expression) extends Expression

    final case class If(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression

    final case class Merge(record: Expression, update: Expression, tipe: Option[Expression]) extends Expression

    final case class ToMap(data: Expression, tipe: Option[Expression]) extends Expression

    final case class EmptyList(tipe: Expression) extends Expression

    final case class NonEmptyList(head: Expression, tail: List[Expression]) extends Expression

    final case class Annotation(data: Expression, tipe: Expression) extends Expression

    final case class Operator(lop: Expression, op: SyntaxConstants.Operator, rop: Expression) extends Expression

    final case class Application(func: Expression, arg: Expression) extends Expression

    final case class Field(data: Expression, name: String) extends Expression

    final case class ProjectByLabels(data: Expression, labels: List[String]) extends Expression


    final case class ProjectByType(data: Expression, by: Expression) extends Expression

    final case class Completion(data: Expression, tipe: Expression) extends Expression

    final case class Assert(assertion: Expression) extends Expression

    final case class With(data: Expression, pathComponents: List[PathComponent], body: Expression) extends Expression


    final case class DoubleLiteral(value: Double) extends Expression


    final case class NaturalLiteral(value: Natural) extends Expression

    final case class IntegerLiteral(value: Int) extends Expression

    final case class TextLiteralNoInterp(value: String) extends Expression

    final case class TextLiteral(interpolations: List[(String, Expression)], trailing: String) extends Expression

    final case class BytesLiteral(value: Array[Byte]) extends Expression

    final case class DateLiteral(date: LocalDate) extends Expression

    final case class TimeLiteral(time: LocalTime) extends Expression

    final case class TimeZoneLiteral(tz: ZoneOffset) extends Expression

    final case class RecordType(defs: List[(String, Expression)]) extends Expression

    final case class RecordLiteral(defs: List[(String, Expression)]) extends Expression

    final case class UnionType(defs: List[(String, Option[Expression])]) extends Expression

    final case class ShowConstructor(data: Expression) extends Expression

    final case class Import(importType: SyntaxConstants.ImportType, importMode: SyntaxConstants.ImportMode, digest: Option[Array[Byte]]) extends Expression

    final case class Some(data: Expression) extends Expression


    final case class Builtin(builtin: SyntaxConstants.Builtin) extends Expression

    final case class Constant(constant: SyntaxConstants.Constant) extends Expression

  }

  sealed trait PathComponent

  object PathComponent {
    final case class Label(dirName: String) extends PathComponent

    final case object DescendOptional extends PathComponent
  }
}
