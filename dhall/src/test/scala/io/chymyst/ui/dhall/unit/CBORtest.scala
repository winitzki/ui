package io.chymyst.ui.dhall.unit

import com.eed3si9n.expecty.Expecty.expect
import com.upokecenter.cbor.CBORObject
import io.chymyst.ui.dhall.{CBOR, CBORmodel, SyntaxConstants}
import io.chymyst.ui.dhall.Syntax.Expression
import io.chymyst.ui.dhall.SyntaxConstants.VarName
import io.chymyst.ui.dhall.unit.CBORtest.cborRoundtrip
import munit.FunSuite

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


  test("CBOR roundtrips") {
    cborRoundtrip(Expression.Builtin(SyntaxConstants.Builtin.True))
    cborRoundtrip(Expression.Builtin(SyntaxConstants.Builtin.List))
    cborRoundtrip(Expression.NaturalLiteral(123))
    cborRoundtrip(Expression.Variable(VarName("_"), BigInt(3)))
    cborRoundtrip(Expression.DoubleLiteral(1.0))
    cborRoundtrip(Expression.TextLiteralNoInterp("abcde"))
    cborRoundtrip(Expression.NonEmptyList(Expression.NaturalLiteral(1), Seq(2, 3, 4, 5).map(x => Expression.NaturalLiteral(x))))
  }
}
