package net.andimiller.parser

import scala.meta._

sealed trait Expr[T]
case class StringLiteral(s: String) extends Expr[String]
case class IntLiteral(i: Int) extends Expr[Int]

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  @inline def foo(i: Int): Int = i * 2
}
