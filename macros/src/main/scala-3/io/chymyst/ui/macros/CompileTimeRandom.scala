package io.chymyst.ui.macros

import java.util.UUID
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import quotes.reflect._

// Macros for generating unique compile-time values.
object CompileTimeRandom {
  inline def uuid: UUID =
  ${     uuidImpl }

  def uuidImpl(using Quotes): Expr[UUID] = {

    given ToExpr[UUID] with{
      def apply(x: UUID)(using Quotes) = '{
        _root_.java.util.UUID.fromString(${
          Expr(x.toString)
        }.toString)
      }
    }
    val uuid = UUID.randomUUID()
     Expr(uuid)

  }

 inline def long: Long = ${ longImpl }

  def longImpl(using Quotes): Expr[Long] = Expr(scala.util.Random.nextLong())

}
