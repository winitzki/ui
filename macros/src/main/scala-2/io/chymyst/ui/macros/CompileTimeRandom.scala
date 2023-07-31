package io.chymyst.ui.macros

import java.util.UUID
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

// Macros for generating unique compile-time values.
object CompileTimeRandom {

  def uuid: UUID = macro uuidImpl

  def long: Long = macro longImpl

  def uuidImpl(c: Context): c.Expr[UUID] = {
    import c.universe._
    implicit val l: Liftable[UUID] = Liftable((in: UUID) => q"_root_.java.util.UUID.fromString(${in.toString})")
    val uuid = java.util.UUID.randomUUID()
    c.Expr[UUID](q"""$uuid""")
  }

  def longImpl(c: Context): c.Expr[Long] = {
    import c.universe._
    val result = scala.util.Random.nextLong()
    c.Expr[Long](q"$result")
  }

}
