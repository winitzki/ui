package io.chymyst.ui.dhall


import com.upokecenter.cbor.{CBORObject, CBORType}
import com.upokecenter.numbers.EInteger
import io.chymyst.ui.dhall.CBORmodel._
import io.chymyst.ui.dhall.Syntax.{Expression, Natural, PathComponent}
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, ImportType, VarName}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.jdk.CollectionConverters.MapHasAsScala

sealed trait CBORmodel {
  def toCBOR: CBORObject

  final def toExpression: Expression = this match {
    case CBORmodel.CNull => ???
    case CBORmodel.CTrue => ???
    case CBORmodel.CFalse => ???
    case CInt(data) => ???
    case CDouble(data) => ???
    case CString(data) => ???
    case CArray(data) => ???
    case CBytes(data) => ???
    case CMap(data) => ???
    case CTagged(4, data) => ???
  }

  def asString: String = throw new Exception(s"This CBORmodel is $this and not a CString")
}

object CBORmodel {

  def fromCbor(obj: CBORObject): CBORmodel = if (obj == null) CNull else {
    val decoded: CBORmodel = obj.getType match {
      case CBORType.Number => ???

      case CBORType.Boolean => obj.getSimpleValue match {
        case 20 => CFalse
        case 21 => CTrue
        case 22 => CNull
        case x => throw new Exception(s"boolean has unexpected simple value $x")
      }

      case CBORType.SimpleValue => obj.getSimpleValue match {
        case 20 => CFalse
        case 21 => CTrue
        case 22 => CNull
        case x => throw new Exception(s"got CBOR simple value $x")
      }

      case CBORType.ByteString => CBytes(obj.GetByteString)
      case CBORType.TextString => CString(obj.AsString)
      case CBORType.Array =>
        val objs: Array[CBORObject] = obj.ToObject(classOf[Array[CBORObject]])
        CArray(objs.map(fromCbor))
      case CBORType.Map =>
        val objs: java.util.Map[CBORObject, CBORObject] = obj.ToObject(classOf[java.util.Map[CBORObject, CBORObject]])
        CMap(objs.asScala.map { case (k, v) => (fromCbor(k).asString, fromCbor(v)) }.toMap)
      case CBORType.Integer =>
        if (obj.CanValueFitInInt64()) CInt(BigInt(obj.AsInt64Value)) else CInt(eIntegerToBigInt(obj.AsEIntegerValue))
      case CBORType.FloatingPoint => CDouble(obj.AsDoubleValue)
    }

    if (obj.isTagged) {
      val tags: Array[Natural] = obj.GetAllTags.map(eIntegerToBigInt)
      if (obj.HasOneTag)
        CTagged(tags(0).toInt, decoded)
      else throw new Exception(s"CBOR object $decoded has more than one tag, this is unsupported by Dhall")
    } else decoded
  }

  def eIntegerToBigInt(eInt: EInteger): BigInt = BigInt(eInt.signum, eInt.ToBytes(false))

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
    override def toCBOR: CBORObject = {
      val result = data match {
        // Important: match -0.0 before 0.0 or else it cannot match.
        case -0.0 => CBORObject.FromObject(java.lang.Double.valueOf(data))//CBORObject.FromFloatingPointBits(0x8000L, 2)
        case 0.0 => CBORObject.FromFloatingPointBits(0L, 2)
        case Double.NaN => CBORObject.FromFloatingPointBits(0x7e00L, 2)
        case Double.NegativeInfinity => CBORObject.FromFloatingPointBits(0xfc00L, 2)
        case Double.PositiveInfinity => CBORObject.FromFloatingPointBits(0x7c00L, 2)
        case _ => CBORObject.FromObject(java.lang.Double.valueOf(data))
      }
      result
    }

    override def toString: String = f"$data%.1f"
  }

  final case class CString(data: String) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def asString: String = data

    override def toString: String = s"\"$escaped\""

    lazy val escaped: String = data.flatMap {
      case '\b' => "\\b"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c if c.toInt > 255 || c.toInt < 20 => s"\\u${c.toHexString.toUpperCase}"
      case c => c.toString
    }
  }

  final case class CArray(data: Array[CBORmodel]) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data.map(_.toCBOR))

    override def toString: String = "[" + data.map(_.toString).mkString(", ") + "]"
  }


  final case class CBytes(data: Array[Byte]) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObject(data)

    override def toString: String = "h'" + data.map(b => String.format("%02X", Byte.box(b))).mkString("") + "'"
  }

  final case class CMap(data: Map[String, CBORmodel]) extends CBORmodel {
    override def toCBOR: CBORObject = {
      val dict = CBORObject.NewOrderedMap
      data.toSeq.sortBy(_._1).foreach { case (k, v) => dict.Add(k, v.toCBOR) } // Dhall requires sorting by the dictionary's keys.
      CBORObject.FromObject(dict)
    }

    override def toString: String = "{" + data.toSeq.sortBy(_._1).map { case (k, v) => s"\"$k\": $v" }.mkString(", ") + "}"
  }

  final case class CTagged(tag: Int, data: CBORmodel) extends CBORmodel {
    override def toCBOR: CBORObject = CBORObject.FromObjectAndTag(data.toCBOR, tag)

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

  def naturalToCbor2(index: Natural): CBORObject =
    if (index < BigInt(1).<<(64))
      CBORObject.FromObject(index.bigInteger)
    else
      CBORObject.FromObject(EInteger.FromBytes(index.toByteArray, false)) // TODO: Does this work correctly? Do we need to set littleEndian = true?

  def toCborModel(e: Expression): CBORmodel = e match {
    case Expression.Variable(VarName("_"), index) => CInt(index)

    case Expression.Variable(VarName(name), index) => array(name, index)

    case Expression.Lambda(VarName(name), tipe, body) => if (name == "_") array(1, tipe, body) else array(1, name, tipe, body)

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

    case Expression.NonEmptyList(head, tail) => array(4 +: null +: head +: tail: _*)

    case Expression.Annotation(data, tipe) => array(26, data, tipe)

    case Expression.Operator(lop, op, rop) => array(3, op.cborCode, lop, rop)

    case f@Expression.Application(_, _) =>
      @tailrec def loop(args: Seq[Expression], expr: Expression): Seq[Expression] = expr match {
        case Expression.Application(f, a) => loop(a +: args, f)
        case _ => expr +: args
      }

      array(0 +: loop(Seq(), f): _*)

    case Expression.Field(base, FieldName(name)) => array(9, base, name)

    case Expression.ProjectByLabels(base, labels) => array(10 +: base +: labels.map(_.name): _*)

    case Expression.ProjectByType(base, by) => array(10, base, array(by))

    case Expression.Completion(base, target) => array(3, 13, base, target)

    case Expression.Assert(data) => array(19, data)

    case Expression.With(data, pathComponents, body) =>
      val path: Seq[Any] = pathComponents.map {
        case PathComponent.Label(FieldName(name)) => name
        case PathComponent.DescendOptional => 0
      }
      array(29, data, array(path: _*), body)

    case Expression.DoubleLiteral(value) => CDouble(value) // TODO: verify that this works correctly.

    case Expression.NaturalLiteral(value) => array(15, value)

    case Expression.IntegerLiteral(value) => array(16, value)

    case Expression.TextLiteralNoInterp(value) => array(18, value)

    case Expression.TextLiteral(interpolations, trailing) =>
      val objects: Seq[Any] = interpolations.flatMap { case (head, tail) => Seq(head, tail) } :+ trailing
      array(18 +: objects: _*)

    case b@Expression.BytesLiteral(_) => array(33, CBytes(b.bytes))

    case Expression.DateLiteral(y, m, d) => array(30, y, m, d)

    case Expression.TimeLiteral(time) =>
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
      val cborSign: CBORmodel = if (isPositive) CTrue else CFalse
      array(32, cborSign, hours, minutes)

    case Expression.RecordType(defs) =>
      val dict = defs
        .map { case (FieldName(name), expr) => (name, expr) }
        .toMap
      array(7, dict)

    case Expression.RecordLiteral(defs) =>
      val dict = defs
        .map { case (FieldName(name), expr) => (name, expr) }
        .toMap
      array(8, dict)

    case e@Expression.RawRecordLiteral(_, _) => toCborModel(Expression.RecordLiteral.of(Seq(e)))

    case Expression.UnionType(defs) =>
      val dict = defs
        .map { case (ConstructorName(name), maybeExpr) => (name, maybeExpr.orNull)
        }.toMap
      array(11, dict)

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
      array(24 +: (part1 ++ part2): _*)

    case Expression.KeywordSome(data) => array(5, null, data)

    case Expression.Builtin(SyntaxConstants.Builtin.True) | Expression.Constant(SyntaxConstants.Constant.True) => CTrue

    case Expression.Builtin(SyntaxConstants.Builtin.False) | Expression.Constant(SyntaxConstants.Constant.False) => CFalse

    case Expression.Builtin(builtin) => CString(builtin.entryName)

    case Expression.Constant(constant) => CString(constant.entryName)
  }

}
