package io.chymyst.ui.dhall


import com.upokecenter.cbor.{CBORObject, CBORType}
import com.upokecenter.numbers.EInteger
import io.chymyst.ui.dhall.Syntax.{Expression, Natural, PathComponent}
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, VarName}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.jdk.CollectionConverters.MapHasAsJava

// See https://github.com/dhall-lang/dhall-lang/blob/master/standard/binary.md
object CBOR {

  def exprToBytes(e: Expression): Array[Byte] = toCbor2(e).EncodeToBytes()

  def bytesToExpr(bytes: Array[Byte]): Expression = fromCbor2(CBORObject.DecodeFromBytes(bytes))

  def fromCbor2(obj: CBORObject): Expression = obj.getType match {
    case CBORType.Number => ???
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
      CBORObject.FromObject(EInteger.FromBytes(index.toByteArray, false)) // TODO: Does this work correctly? Do we need to set littleEndian = true?

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

    case e@Expression.Let(_, _, _, _) =>
      @tailrec def loop(acc: Seq[CBORObject], expr: Expression): Seq[CBORObject] = expr match {
        case Expression.Let(VarName(name), tipe, subst, body) => loop((acc :+ CBORObject.FromObject(name)) ++ tipe.map(toCbor2).toSeq :+ toCbor2(subst), body)
        case _ => acc ++ Seq(toCbor2(expr))
      }

      makeArrayC(Some(25))(loop(Seq(), e): _*)

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

    case Expression.Field(base, FieldName(name)) => makeArrayC(Some(9))(toCbor2(base), CBORObject.FromObject(name))
    case Expression.ProjectByLabels(base, labels) => makeArrayC(Some(10))(toCbor2(base) +: labels.map(label => CBORObject.FromObject(label.name)): _*)
    case Expression.ProjectByType(base, by) => makeArrayC(Some(10))(toCbor2(base), makeArrayC()(toCbor2(by)))
    case Expression.Completion(base, target) => makeArray(Some(3), Some(13))(base, target)
    case Expression.Assert(data) => makeArray(Some(19))(data)

    case Expression.With(data, pathComponents, body) =>
      val path: Seq[CBORObject] = pathComponents.map {
        case PathComponent.Label(FieldName(name)) => CBORObject.FromObject(name)
        case PathComponent.DescendOptional => CBORObject.FromObject(0)
      }
      makeArrayC(Some(29))(toCbor2(data), makeArrayC()(path: _*), toCbor2(body))

    case Expression.DoubleLiteral(value) => CBORObject.FromObject(value) // TODO: verify that this works correctly.
    case Expression.NaturalLiteral(value) => makeArrayC(Some(15))(naturalToCbor2(value))
    case Expression.IntegerLiteral(value) => makeArrayC(Some(16))(CBORObject.FromObject(value)) // TODO: verify that this works correctly.
    case Expression.TextLiteralNoInterp(value) => makeArrayC(Some(18))(CBORObject.FromObject(value))

    case Expression.TextLiteral(interpolations, trailing) =>
      val objects: Seq[CBORObject] =
        interpolations
          .flatMap { case (head, tail) => Seq(CBORObject.FromObject(head), toCbor2(tail)) } :+ CBORObject.FromObject(trailing)
      makeArrayC(Some(18))(objects: _*)

    case b@Expression.BytesLiteral(_) => makeArrayC(Some(33))(CBORObject.FromObject(b.bytes))
    case Expression.DateLiteral(y, m, d) => makeArrayC(Some(30))(Seq(y, m, d).map(x => CBORObject.FromObject(x)): _*)
    case Expression.TimeLiteral(time) =>
      // Always use nanosecond precision. TODO: validate that this is OK
      val totalSeconds: Long = time.getSecond * 1000000000 + time.getNano
      makeArrayC(Some(31))(
        CBORObject.FromObject(time.getHour),
        CBORObject.FromObject(time.getMinute),
        CBORObject.FromObjectAndTag(makeArrayC()(CBORObject.FromObject(-9), CBORObject.FromObject(totalSeconds)), 4)
      )

    case Expression.TimeZoneLiteral(totalMinutes) =>
      val hours: Int = math.abs(totalMinutes) / 60
      val minutes: Int = math.abs(totalMinutes) % 60
      val isPositive: Boolean = totalMinutes >= 0
      val cborSign = if (isPositive) CBORObject.True else CBORObject.False
      makeArrayC(Some(32))(cborSign, CBORObject.FromObject(hours), CBORObject.FromObject(minutes))

    case Expression.RecordType(defs) => ???
    case Expression.RecordLiteral(defs) => ???

    case e@Expression.RawRecordLiteral(_, _) => toCbor2(Expression.RecordLiteral.of(Seq(e))) // This is a type defined at parse time and should not occur here.

    case Expression.UnionType(defs) =>
      val dict: java.util.Map[String, CBORObject] = defs.map {
        case (ConstructorName(name), Some(expr)) => (name, toCbor2(expr))
        case (ConstructorName(name), None) => (name, CBORObject.Null)
      }.toMap.asJava

      makeArrayC(Some(11))(CBORObject.FromObject(dict))

    case Expression.ShowConstructor(data) => makeArray(Some(34))(data)
    case Expression.Import(importType, importMode, digest) => ???
    case Expression.KeywordSome(data) => makeArray(Some(5), None)(data)
    case Expression.Builtin(SyntaxConstants.Builtin.True) | Expression.Constant(SyntaxConstants.Constant.True) => CBORObject.True
    case Expression.Builtin(SyntaxConstants.Builtin.False) | Expression.Constant(SyntaxConstants.Constant.False) => CBORObject.False
    case Expression.Builtin(builtin) => CBORObject.FromObject(builtin.entryName)
    case Expression.Constant(constant) => CBORObject.FromObject(constant.entryName)

  }

}
