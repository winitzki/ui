package io.chymyst.ui.dhall


import com.upokecenter.cbor.{CBORObject, CBORType}
import com.upokecenter.numbers.EInteger
import io.chymyst.ui.dhall.Syntax.{Expression, Natural, PathComponent}
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, FilePrefix, ImportType, VarName}
import CBORmodel._
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.jdk.CollectionConverters.MapHasAsJava

sealed trait CBORmodel {
  def toCBOR: CBORObject

  final def toExpression: Expression = ???
}

object CBORmodel {
  final case object CNull extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.Null

    override def toString: String = "null"
  }

  final case object CTrue extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.True

    override def toString: String = "true"
  }

  final case object CFalse extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.False

    override def toString: String = "false"
  }

  // Either a 64-bit int or a bigint.
  final case class CInt(data: BigInt) extends CBORmodel {
    override def toCBOR: CBORObject = CBOR.naturalToCbor2(data)

    override def toString: String = data.toString
  }

  final case class CDouble(data: Double) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def toString: String = f"$data%.1f"
  }

  final case class CString(data: String) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def toString: String = s"\"$data\""
  }

  final case class CArray(data: Array[CBORmodel]) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def toString: String = "[" + data.map(_.toString).mkString(", ") + "]"
  }


  final case class CBytes(data: Array[Byte]) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def toString: String = "h'" + data.map(b => String.format("%02X", Byte.box(b))).mkString("") + "'"
  }

  final case class CMap(data: Map[String, CBORmodel]) extends CBORmodel {
    override def toCBOR: CBORObject = {
      val dict: java.util.Map[String, CBORObject] = data.map { case (k, v) => (k, v.toCBOR) }.asJava
      CBORObject.FromObject(dict)
    }

    override def toString: String = "{" + data.toSeq.sortBy(_._1.toString).map { case (k, v) => s"\"$k\": $v" }.mkString(", ") + "}"
  }

  final case class CTagged(tag: Int, data: CBORmodel) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObjectAndTag(data, tag)

    override def toString: String = s"$tag($data)"
  }

  def toCBORmodel: Any => CBORmodel = {
    case null => CNull
    case true => CTrue
    case false => CFalse
    case x: Int => CInt(BigInt(x))
    case x: Long => CInt(BigInt(x))
    case x: BigInt => CInt(x)
    case x: String => CString(x)
    case x: Array[Any] => CArray(x.map(toCBORmodel))
    case x: Map[String, Any] => CMap(x.map { case (k, v) => (k, toCBORmodel(v)) })
    case x: Expression => CBOR.toCborModel(x)
    case x: CBORmodel => x
    case x => throw new Exception(s"Invalid input toCBORmodel($x:${x.getClass})")
  }

  def array(objs: Any*): CArray = CArray(Array(objs: _*).map(toCBORmodel))

}

// See https://github.com/dhall-lang/dhall-lang/blob/master/standard/binary.md
object CBOR {

  def exprToBytes(e: Expression): Array[Byte] = toCborModel(e).toCBOR.EncodeToBytes()

  def bytesToExpr(bytes: Array[Byte]): Expression = fromCbor(CBORObject.DecodeFromBytes(bytes)).toExpression

  def fromCbor(obj: CBORObject): CBORmodel = obj.getType match {
    case CBORType.Number => ???
    case CBORType.Boolean => ???
    case CBORType.SimpleValue => ???
    case CBORType.ByteString => ???
    case CBORType.TextString => ???
    case CBORType.Array => ???
    case CBORType.Map => ???
    case CBORType.Integer =>
      ???
    case CBORType.FloatingPoint => ???
  }

  def naturalToCbor2(index: Natural): CBORObject =
    if (index < BigInt(1).<<(64))
      CBORObject.FromObject(index)
    else
      CBORObject.FromObject(EInteger.FromBytes(index.toByteArray, false)) // TODO: Does this work correctly? Do we need to set littleEndian = true?

  def toCborModel(e: Expression): CBORmodel = e match {
    case Expression.Variable(VarName("_"), index) => CInt(index) // naturalToCbor2(index)

    case Expression.Variable(VarName(name), index) => array(name, index) //CBORObject.NewArray().Add(name).Add(naturalToCbor2(index))

    case Expression.Lambda(VarName(name), tipe, body) => if (name == "_") array(1, tipe, body) else array(1, name, tipe, body) //makeArray(Some(1))(tipe, body)

    case Expression.Forall(VarName(name), tipe, body) => if (name == "_") array(2, tipe, body) else array(2, name, tipe, body)

    case e@Expression.Let(_, _, _, _) =>
      @tailrec def loop(acc: Seq[Any], expr: Expression): Seq[Any] = expr match {
        case Expression.Let(VarName(name), tipe, subst, body) => loop((acc :+ name) :+ tipe.orNull :+ subst, body)
        case _ => acc :+ expr
      }

      array(25 +: loop(Seq(), e): _*)

    case Expression.If(cond, ifTrue, ifFalse) => array(14, cond, ifTrue, ifFalse)

    case Expression.Merge(record, update, tipe) =>
      val args: Seq[Expression] = Seq(record, update) ++ tipe.toSeq
      array(6 +: args: _*)

    case Expression.ToMap(data, tipe) =>
      val args: Seq[Expression] = Seq(data) ++ tipe.toSeq
      array(27 +: args: _*)

    case Expression.EmptyList(Expression.Application(Expression.Builtin(SyntaxConstants.Builtin.List), tipe)) => array(4, tipe)

    case Expression.EmptyList(tipe) => array(28, tipe)

    case Expression.NonEmptyList(head, tail) => array(4 +: null +: head +: tail: _*) //makeArray(Some(4), None)((head +: tail): _*)

    case Expression.Annotation(data, tipe) => array(26, data, tipe) //makeArray(Some(26))(data, tipe)

    case Expression.Operator(lop, op, rop) => array(3, op.cborCode, lop, rop) //makeArray(Some(3), Some(op.cborCode))(lop, rop)

    case f@Expression.Application(_, _) =>
      @tailrec def loop(args: Seq[Expression], expr: Expression): Seq[Expression] = expr match {
        case Expression.Application(f, a) => loop(a +: args, f)
        case _ => expr +: args
      }

      array(0 +: loop(Seq(), f): _*) //makeArray(Some(0))(loop(Seq(), f): _*)

    case Expression.Field(base, FieldName(name)) => array(9, base, name) //makeArrayC(Some(9))(toCborModel(base), CBORObject.FromObject(name))

    case Expression.ProjectByLabels(base, labels) => array(10 +: base +: labels.map(_.name): _*) //makeArrayC(Some(10))(toCborModel(base) +: labels.map(label => CBORObject.FromObject(label.name)): _*)

    case Expression.ProjectByType(base, by) => array(10, base, array(by)) // makeArrayC(Some(10))(toCborModel(base), makeArrayC()(toCborModel(by)))

    case Expression.Completion(base, target) => array(3, 13, base, target) //makeArray(Some(3), Some(13))(base, target)

    case Expression.Assert(data) => array(19, data) //makeArray(Some(19))(data)

    case Expression.With(data, pathComponents, body) =>
      val path: Seq[Any] = pathComponents.map {
        case PathComponent.Label(FieldName(name)) => name
        case PathComponent.DescendOptional => 0
      }
      array(29, data, array(path: _*), body) //makeArrayC(Some(29))(toCborModel(data), makeArrayC()(path: _*), toCborModel(body))

    case Expression.DoubleLiteral(value) => CDouble(value) // CBORObject.FromObject(value) // TODO: verify that this works correctly.

    case Expression.NaturalLiteral(value) => array(15, value) // makeArrayC(Some(15))(naturalToCbor2(value))

    case Expression.IntegerLiteral(value) => array(16, value) //makeArrayC(Some(16))(CBORObject.FromObject(value)) // TODO: verify that this works correctly.

    case Expression.TextLiteralNoInterp(value) => array(18, value) // makeArrayC(Some(18))(CBORObject.FromObject(value))

    case Expression.TextLiteral(interpolations, trailing) =>
      val objects: Seq[Any] =
        interpolations.flatMap { case (head, tail) => Seq(head, tail) } :+ trailing

      array(18 +: objects: _*)
    //makeArrayC(Some(18))(objects: _*)

    case b@Expression.BytesLiteral(_) => array(33, CBytes(b.bytes)) // makeArrayC(Some(33))(CBORObject.FromObject(b.bytes))

    case Expression.DateLiteral(y, m, d) => array(30, y, m, d) //makeArrayC(Some(30))(Seq(y, m, d).map(x => CBORObject.FromObject(x)): _*)

    case Expression.TimeLiteral(time) =>
      // Always use nanosecond precision. TODO: this is not what the standard tests do, need to fix
      @tailrec def getPrecision(nanos: Long, initPrecision: Int): Int =
        if (nanos <= 0) 0
        else if (nanos % 10 > 0) initPrecision
        else getPrecision(nanos / 10, initPrecision - 1)

      val precision = getPrecision(time.getNano, 9)
      val totalSeconds: Long = (time.getSecond * math.pow(10, precision).toLong + time.getNano) / math.pow(10, precision).toLong
      array(31, time.getHour, time.getMinute, CTagged(4, array(precision, totalSeconds)))

    case Expression.TimeZoneLiteral(totalMinutes) =>
      val hours: Int = math.abs(totalMinutes) / 60
      val minutes: Int = math.abs(totalMinutes) % 60
      val isPositive: Boolean = totalMinutes >= 0
      val cborSign = if (isPositive) CTrue else CFalse
      array(32, cborSign, hours, minutes)
    //makeArrayC(Some(32))(cborSign, CBORObject.FromObject(hours), CBORObject.FromObject(minutes))

    case Expression.RecordType(defs) => // TODO: {,} must be parsed as a record type, not literal, while {=} is an empty record literal.
      val dict = defs
        .map { case (FieldName(name), expr) => (name, expr) }
        .toMap
      array(7, dict) //     makeArrayC(Some(7))(CBORObject.FromObject(dict))

    case Expression.RecordLiteral(defs) =>
      val dict = defs
        .map { case (FieldName(name), expr) => (name, expr) }
        .toMap
      array(8, dict) //makeArrayC(Some(8))(CBORObject.FromObject(dict))

    case e@Expression.RawRecordLiteral(_, _) => toCborModel(Expression.RecordLiteral.of(Seq(e))) // This is a type defined at parse time and should not occur here.

    case Expression.UnionType(defs) =>
      val dict = defs
        .map { case (ConstructorName(name), maybeExpr) => (name, maybeExpr.orNull)
        }.toMap
      array(11, dict) //makeArrayC(Some(11))(CBORObject.FromObject(dict))

    case Expression.ShowConstructor(data) => array(34, data)

    case Expression.Import(importType, importMode, digest) =>
      val integrity = digest.map(d => CBytes("\u0012\u0020".getBytes ++ d.bytes)).orNull
      val part1 = Seq(integrity, importMode.cborCode)
      val part2 = importType match {
        case ImportType.Missing => Seq(7)

        case ImportType.Remote(SyntaxConstants.URL(scheme, authority, SyntaxConstants.File(segments), query), headers) =>
          val pathSegmentsForCbor = if (segments.isEmpty) Seq("") else segments
          scheme.cborCode +: headers.orNull +: authority +: pathSegmentsForCbor :+ query.orNull

        case ImportType.Path(filePrefix, SyntaxConstants.File(segments)) => filePrefix.cborCode +: segments

        case ImportType.Env(envVarName) => Seq(6, envVarName)
      }
      array(24 +: (part1 ++ part2): _*) // makeArrayC(Some(24))(part1 ++ part2: _*)

    case Expression.KeywordSome(data) => array(5, null, data) //makeArray(Some(5), None)(data)

    case Expression.Builtin(SyntaxConstants.Builtin.True) | Expression.Constant(SyntaxConstants.Constant.True) => CTrue

    case Expression.Builtin(SyntaxConstants.Builtin.False) | Expression.Constant(SyntaxConstants.Constant.False) => CFalse

    case Expression.Builtin(builtin) => CString(builtin.entryName)

    case Expression.Constant(constant) => CString(constant.entryName)

  }

}
