package net.andimiller

import net.andimiller.parser.Macros

object MacroTest {
  def main(args: Array[String]): Unit = {
    println(Macros.foo(123))
  }
}
