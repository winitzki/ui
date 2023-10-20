package io.chymyst.ui.dhall


import com.upokecenter.cbor.{CBORObject, CBORType}
import com.upokecenter.numbers.EInteger
import io.chymyst.ui.dhall.Syntax.{Expression, Natural}
import io.chymyst.ui.dhall.SyntaxConstants.VarName

import scala.annotation.tailrec
import scala.collection.immutable.Seq

object CBOR {

  def exprToBytes(e: Expression): Array[Byte] = toCbor2(e).EncodeToBytes()

  def bytesToExpr(bytes: Array[Byte]): Expression = fromCbor2(CBORObject.DecodeFromBytes(bytes))

  def fromCbor2(obj: CBORObject): Expression = obj.getType match {
    case CBORType.Boolean => ???
    case CBORType.SimpleValue => ???
    case CBORType.ByteString => ???
    case CBORType.TextString => ???
    case CBORType.Array => ???
    case CBORType.Map => ???
    case CBORType.Integer => // Either a 64-bit int or a bigint.
      ???
    case CBORType.FloatingPoint => ???
  }

  def naturalToCbor2(index: Natural): CBORObject =
    if (index < BigInt(1).<<(64))
      CBORObject.FromObject(index)
    else
      CBORObject.FromObject(EInteger.FromBytes(index.toByteArray, false))

  private def makeArray(codes: Option[Int]*)(exprs: Expression*): CBORObject = makeArrayC(codes: _*)(exprs.map(e => toCbor2(e)): _*)

  private def makeArrayC(codes: Option[Int]*)(exprs: CBORObject*): CBORObject = {
    val cborCodes = codes.toSeq.map {
      case Some(i: Int) => CBORObject.FromObject(i)
      case None => CBORObject.Null
    }
    val cborExprs = exprs.toSeq
    CBORObject.FromObject((cborCodes ++ cborExprs).toArray)
  }

  def toCbor2(e: Expression): CBORObject = e match {
    case Expression.Variable(VarName("_"), index) => naturalToCbor2(index)
    case Expression.Variable(VarName(name), index) => CBORObject.NewArray().Add(name).Add(naturalToCbor2(index))
    case Expression.Lambda(VarName(name), tipe, body) => makeArray(Some(1))(tipe, body)
    case Expression.Forall(VarName(name), tipe, body) => makeArray(Some(2))(tipe, body)
    case Expression.Let(name, tipe, subst, body) => ???
    case Expression.If(cond, ifTrue, ifFalse) => makeArray(Some(14))(cond, ifTrue, ifFalse)
    case Expression.Merge(record, update, tipe) =>
      val args: Seq[Expression] = Seq(record, update) ++ tipe.toSeq
      makeArray(Some(6))(args: _*)
    case Expression.ToMap(data, tipe) =>
      val args: Seq[Expression] = Seq(data) ++ tipe.toSeq
      makeArray(Some(27))(args: _*)
    case Expression.EmptyList(Expression.Application(Expression.Builtin(SyntaxConstants.Builtin.List), tipe)) => makeArray(Some(4))(tipe)
    case Expression.EmptyList(tipe) => makeArray(Some(28))(tipe)
    case Expression.NonEmptyList(head, tail) => makeArray(Some(4), None)((head +: tail): _*)
    case Expression.Annotation(data, tipe) => makeArray(Some(26))(data, tipe)
    case Expression.Operator(lop, op, rop) => makeArray(Some(3), Some(op.cborCode))(lop, rop)
    case f@Expression.Application(_, _) =>
      @tailrec def loop(args: Seq[Expression], expr: Expression): Seq[Expression] = expr match {
        case Expression.Application(f, a) => loop(a +: args, f)
        case _ => expr +: args
      }

      makeArray(Some(0))(loop(Seq(), f): _*)
    case Expression.Field(base, name) => ???
    case Expression.ProjectByLabels(base, labels) => ???
    case Expression.ProjectByType(base, by) => ???
    case Expression.Completion(base, target) => makeArray(Some(3), Some(13))(base, target)
    case Expression.Assert(data) => makeArray(Some(19))(data)
    case Expression.With(data, pathComponents, body) => ???
    case Expression.DoubleLiteral(value) => ???
    case Expression.NaturalLiteral(value) => makeArrayC(Some(15))(naturalToCbor2(value))
    case Expression.IntegerLiteral(value) => ???
    case Expression.TextLiteralNoInterp(value) => makeArrayC(Some(18))(CBORObject.FromObject(value))
    case Expression.TextLiteral(interpolations, trailing) =>
      val objects: Seq[CBORObject] = interpolations.flatMap { case (head, tail) => Seq(CBORObject.FromObject(head), toCbor2(tail)) } :+ CBORObject.FromObject(trailing)
      makeArrayC(Some(18))(objects: _*)
    case Expression.BytesLiteral(hex) => ???
    case Expression.DateLiteral(date) => ???
    case Expression.TimeLiteral(time) => ???
    case Expression.TimeZoneLiteral(tz) => ???
    case Expression.RecordType(defs) => ???
    case Expression.RecordLiteral(defs) => ???
    case Expression.RawRecordLiteral(base, defs) => ???
    case Expression.UnionType(defs) => ???
    case Expression.ShowConstructor(data) => makeArray(Some(34))(data)
    case Expression.Import(importType, importMode, digest) => ???
    case Expression.KeywordSome(data) => makeArray(Some(5), None)(data)
    case Expression.Builtin(SyntaxConstants.Builtin.True) | Expression.Constant(SyntaxConstants.Constant.True) => CBORObject.True
    case Expression.Builtin(SyntaxConstants.Builtin.False) | Expression.Constant(SyntaxConstants.Constant.False) => CBORObject.False
    case Expression.Builtin(builtin) => CBORObject.FromObject(builtin.entryName)
    case Expression.Constant(constant) => CBORObject.FromObject(constant.entryName)

  }

}
