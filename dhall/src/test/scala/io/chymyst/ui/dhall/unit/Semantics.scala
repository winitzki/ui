package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.CBOR
import io.chymyst.ui.dhall.Syntax.ExpressionScheme._
import io.chymyst.ui.dhall.Syntax.{Expression, ExpressionScheme, Natural}
import io.chymyst.ui.dhall.SyntaxConstants.{File, VarName}

object Semantics {

  final case class GammaTypeContext(defs: Seq[(VarName, Expression)])

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/shift.md
  def shift(positive: Boolean, x: VarName, minIndex: Natural, expr: Expression): Expression = {
    val d = if (positive) 1 else -1
    expr.scheme match {
      case Variable(name, index) => if (name != x || index < minIndex) expr else Variable(name, index + d)

      case Lambda(name, tipe, body) =>
        val newMinIndex = if (name != x) minIndex else minIndex + 1
        Lambda(name, shift(positive, x, minIndex, tipe), shift(positive, x, newMinIndex, body))

      case Forall(name, tipe, body) =>
        val newMinIndex = if (name != x) minIndex else minIndex + 1
        Forall(name, shift(positive, x, minIndex, tipe), shift(positive, x, newMinIndex, body))

      case Let(name, tipe, subst, body) =>
        val newMinIndex = if (name != x) minIndex else minIndex + 1
        Let(name, tipe.map(shift(positive, x, minIndex, _)), shift(positive, x, minIndex, subst), shift(positive, x, newMinIndex, body))

      case other => other.map(expression => shift(positive, x, minIndex, expression))
    }
  }

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/substitution.md
  def substitute(expr: Expression, substVar: VarName, substIndex: Natural, substTarget: Expression): Expression = expr.scheme match {
    case v@Variable(_, _) => if (v.name == substVar && v.index == substIndex) substTarget else expr

    case Lambda(name, tipe, body) =>
      val newIndex = if (name != substVar) substIndex else substIndex + 1
      val newType = substitute(tipe, name, substIndex, substTarget)
      val newTarget = shift(true, name, 0, substTarget)
      val newBody = substitute(body, name, newIndex, newTarget)
      Lambda(name, newType, newBody)

    case Forall(name, tipe, body) =>
      val newIndex = if (name != substVar) substIndex else substIndex + 1
      val newType = substitute(tipe, name, substIndex, substTarget)
      val newTarget = shift(true, name, 0, substTarget)
      val newBody = substitute(body, name, newIndex, newTarget)
      Forall(name, newType, newBody)

    case Let(name, tipe, subst, body) =>
      val newIndex = if (name != substVar) substIndex else substIndex + 1
      val newType = tipe.map(substitute(_, name, substIndex, substTarget))
      val newSubst = substitute(subst, name, substIndex, substTarget)
      val newTarget = shift(true, name, 0, substTarget)
      val newBody = substitute(body, name, newIndex, newTarget)
      Let(name, newType, newSubst, newBody)

    case other => other.map(expression => substitute(expression, substVar, substIndex, substTarget))
  }

  // TODO: can we reuse this VarName elsewhere instead of using the string "_"?
  val underscore = VarName("_")

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/alpha-normalization.md
  def alphaNormalize(expr: Expression): Expression = expr.scheme match {
    case Variable(_, _) => expr

    case Lambda(name, tipe, body) => if (name == underscore) expr.map(alphaNormalize) else {
      val body1 = shift(true, underscore, 0, body)
      val body2 = substitute(body1, name, 0, Variable(underscore, 0))
      val body3 = shift(false, name, 0, body2)
      Lambda(underscore, alphaNormalize(tipe), alphaNormalize(body3))
    }

    case Forall(name, tipe, body) => if (name == underscore) expr.map(alphaNormalize) else {
      val body1 = shift(true, underscore, 0, body)
      val body2 = substitute(body1, name, 0, Variable(underscore, 0))
      val body3 = shift(false, name, 0, body2)
      Forall(underscore, alphaNormalize(tipe), alphaNormalize(body3))
    }

    case Let(name, tipe, subst, body) => if (name == underscore) expr.map(alphaNormalize) else {
      val body1 = shift(true, underscore, 0, body)
      val body2 = substitute(body1, name, 0, Variable(underscore, 0))
      val body3 = shift(false, name, 0, body2)
      Let(underscore, tipe.map(alphaNormalize), alphaNormalize(subst), alphaNormalize(body3))
    }

    case Import(_, _, _) => throw new Exception(s"alphaNormalize($expr): Imports cannot be α-normalized")
    
    case other => other.map(alphaNormalize)
  }

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/beta-normalization.md
  def betaNormalize(expr: Expression): Expression = ???

  // https://github.com/dhall-lang/dhall-lang/blob/master/standard/equivalence.md
  def equivalence(x: Expression, y: Expression): Boolean =
    CBOR.exprToBytes(betaNormalize(alphaNormalize(x))) sameElements CBOR.exprToBytes(betaNormalize(alphaNormalize(y)))

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/type-inference.md
  def inferType(gamma: GammaTypeContext, expr: Expression): Expression = ???

  // See https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md
  def canonicalize(x: File): File = ???
  // TODO: implement other functions for import handling

}
