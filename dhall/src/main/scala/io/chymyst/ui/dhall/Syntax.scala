package io.chymyst.ui.dhall

import enumeratum._


object Syntax {
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

  sealed trait Expression

  object Expression {
    final case class Variable(name: String, index: Int)
    final case class Lambda(name: String, tipe: Expression, body: Expression)
    final case class Forall(name: String, tipe: Expression, body: Expression)
    final case class Let(name: String, tipe: Option[Expression], subst: Expression, body: Expression)
    final case class If(cond: Expression, ifTrue: Expression, ifFalse: Expression)
  }
  /*
  data Expression
      = Variable Text Natural
        -- ^ > x@n
      | Lambda Text Expression Expression
        -- ^ > λ(x : A) → b
      | Forall Text Expression Expression
        -- ^ > ∀(x : A) → B
      | Let Text (Maybe Expression) Expression Expression
        -- ^ > let x : A = a in b
        --   > let x     = a in b
      | If Expression Expression Expression
        -- ^ > if t then l else r
      | Merge Expression Expression (Maybe Expression)
        -- ^ > merge t u : T
        -- ^ > merge t u
      | ToMap Expression (Maybe Expression)
        -- ^ > toMap t : T
        -- ^ > toMap t
      | EmptyList Expression
        -- ^ > [] : T
      | NonEmptyList (NonEmpty Expression)
        -- ^ > [ t, ts… ]
      | Annotation Expression Expression
        -- ^ > t : T
      | Operator Expression Operator Expression
        -- ^ > l □ r
      | Application Expression Expression
        -- ^ > f a
      | Field Expression Text
        -- ^ > t.x
      | ProjectByLabels Expression [Text]
        -- ^ > t.{ xs… }
      | ProjectByType Expression Expression
        -- ^ > t.(s)
      | Completion Expression Expression
        -- ^ > T::r
      | Assert Expression
        -- ^ > assert : T
      | With Expression (NonEmpty PathComponent) Expression
        -- ^ > e with k.ks… = v
      | DoubleLiteral Double
        -- ^ > n.n
      | NaturalLiteral Natural
        -- ^ > n
      | IntegerLiteral Integer
        -- ^ > ±n
      | TextLiteral TextLiteral
        -- ^ > "s"
        --   > "s${t}ss…"
      | BytesLiteral ByteString
        -- ^ > 0x"abcdef0123456789"
      | DateLiteral Time.Day
      | TimeLiteral
          Time.TimeOfDay
          Int
          -- ^ Precision
      | TimeZoneLiteral Time.TimeZone
      | RecordType [(Text, Expression)]
        -- ^ > {}
        --   > { k : T, ks… }
      | RecordLiteral [(Text, Expression)]
        -- ^ > {=}
        --   > { k = t, ks… }
      | UnionType [(Text, Maybe Expression)]
        -- ^ > <>
        --   > < k : T | ks… >
        --   > < k | ks… >
      | ShowConstructor Expression
        -- ^ > showConstructor t
      | Import ImportType ImportMode (Maybe (Digest SHA256))
      | Some Expression
        -- ^ > Some s
      | Builtin Builtin
      | Constant Constant
   */

  final case class TextLiteral(interpolations: List[(String, Expression)], trailing: String)
}
