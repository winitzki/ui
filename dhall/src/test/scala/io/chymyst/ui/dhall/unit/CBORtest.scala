package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import com.upokecenter.cbor.CBORObject
import io.chymyst.ui.dhall.CBORmodel.{CMap, CString, CTagged}
import io.chymyst.ui.dhall.{CBOR, CBORmodel, SyntaxConstants}
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.SyntaxConstants.VarName
import io.chymyst.ui.dhall.unit.CBORtest.{bytesToCBORmodel, cborRoundtrip}
import munit.FunSuite

import java.time.LocalTime

object CBORtest {
  def bytesToCBORmodel(bytes: Array[Byte]): CBORmodel = CBORmodel.fromCbor(CBORObject.DecodeFromBytes(bytes))

  def cborRoundtrip(expr: Expression) = {
    val aModel = CBOR.toCborModel(expr)

    val aBytes: Array[Byte] = aModel.toCBOR.EncodeToBytes
    val bModel: CBORmodel = bytesToCBORmodel(aBytes)

    val aModelString = aModel.toString
    val bModelString = bModel.toString
    expect(aModelString == bModelString)
  }
}

class CBORtest extends FunSuite {


  test("CBOR roundtrips 1") {
    cborRoundtrip(Expression.Builtin(SyntaxConstants.Builtin.True))
    cborRoundtrip(Expression.Builtin(SyntaxConstants.Builtin.List))
  }

  test("CBOR roundtrips 2") {
    cborRoundtrip(Expression.NaturalLiteral(123))
    cborRoundtrip(Expression.DoubleLiteral(456.0))
    cborRoundtrip(Expression.DoubleLiteral(0.0))
    cborRoundtrip(Expression.DoubleLiteral(-0.0))
    cborRoundtrip(Expression.DoubleLiteral(Double.NaN))
    cborRoundtrip(Expression.DoubleLiteral(Double.NegativeInfinity))
    cborRoundtrip(Expression.DoubleLiteral(Double.PositiveInfinity))
  }

  test("CBOR roundtrips 3") {
    cborRoundtrip(Expression.TextLiteralNoInterp("abcde"))
  }

  test("CBOR roundtrips 4") {
    cborRoundtrip(Expression.NonEmptyList(Expression.NaturalLiteral(1), Seq(2, 3, 4, 5).map(x => Expression.NaturalLiteral(x))))
  }

  test("CBOR roundtrips 5") {
    cborRoundtrip(Expression.Variable(VarName("_"), BigInt(7)))
  }

  test("CBOR roundtrips 6") {
    cborRoundtrip(Expression.TimeLiteral(LocalTime.of(12, 0)))
  }

  test("CBOR for dictionaries") {
    val dict = CMap(Map("a" -> CString("b")))
    val bytes = dict.toCBOR.EncodeToBytes
    val dictAfterBytes = bytesToCBORmodel(bytes)
    expect(dict == dictAfterBytes)
  }

  test("CBOR for tagged array") {
    val taggedDict = CTagged(4, CMap(Map("a" -> CString("b"))))
    val bytes = taggedDict.toCBOR.EncodeToBytes
    val dictAfterBytes = bytesToCBORmodel(bytes)
    expect(taggedDict == dictAfterBytes)
  }
}
