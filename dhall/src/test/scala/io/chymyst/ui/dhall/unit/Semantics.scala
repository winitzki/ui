package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.CBOR
import io.chymyst.ui.dhall.Syntax.{Expression, Natural}
import io.chymyst.ui.dhall.SyntaxConstants.{File, VarName}

object Semantics {

  final case class GammaTypeContext(defs: Seq[(VarName, Expression)])

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/shift.md
  def shift(positive: Boolean, x: VarName, minIndex: Natural, expr: Expression): Expression = expr match {
    case Expression.Variable(name, index) => ???
    case Expression.Lambda(name, tipe, body) => ???
    case Expression.Forall(name, tipe, body) => ???
    case Expression.Let(name, tipe, subst, body) => ???
    case Expression.If(cond, ifTrue, ifFalse) => ???
    case Expression.Merge(record, update, tipe) => ???
    case Expression.ToMap(data, tipe) => ???
    case Expression.EmptyList(tipe) => ???
    case Expression.NonEmptyList(head, tail) => ???
    case Expression.Annotation(data, tipe) => ???
    case Expression.Operator(lop, op, rop) => ???
    case Expression.Application(func, arg) => ???
    case Expression.Field(base, name) => ???
    case Expression.ProjectByLabels(base, labels) => ???
    case Expression.ProjectByType(base, by) => ???
    case Expression.Completion(base, target) => ???
    case Expression.Assert(assertion) => ???
    case Expression.With(data, pathComponents, body) => ???
    case Expression.DoubleLiteral(value) => ???
    case Expression.NaturalLiteral(value) => ???
    case Expression.IntegerLiteral(value) => ???
    case Expression.TextLiteralNoInterp(value) => ???
    case Expression.TextLiteral(interpolations, trailing) => ???
    case Expression.BytesLiteral(hex) => ???
    case Expression.DateLiteral(year, month, day) => ???
    case Expression.TimeLiteral(time) => ???
    case Expression.TimeZoneLiteral(totalMinutes) => ???
    case Expression.RecordType(defs) => ???
    case Expression.RecordLiteral(defs) => ???
    case Expression.UnionType(defs) => ???
    case Expression.ShowConstructor(data) => ???
    case Expression.Import(importType, importMode, digest) => ???
    case Expression.KeywordSome(data) => ???
    case Expression.Builtin(builtin) => ???
    case Expression.Constant(constant) => ???
  }

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/substitution.md
  def substitute(expr: Expression, x: Expression.Variable, body: Expression): Expression = ???

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/alpha-normalization.md
  def alphaNormalize(expr: Expression): Expression = ???

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/beta-normalization.md
  def betaNormalize(expr: Expression): Expression = ???

  // https://github.com/dhall-lang/dhall-lang/blob/master/standard/equivalence.md
  def equivalence(x: Expression, y: Expression): Boolean =
    CBOR.exprToBytes(betaNormalize(alphaNormalize(x))) sameElements CBOR.exprToBytes(betaNormalize(alphaNormalize(y)))

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/type-inference.md
  def inferType(gamma:GammaTypeContext, expr: Expression): Expression = ???

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md
  def canonicalize(x: File): File = ???
  // TODO: implement other functions for import handling

}
