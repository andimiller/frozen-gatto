package net.andimiller

import cats._, cats.implicits._
import net.andimiller.Calculator.Expression.Add
import net.andimiller.Calculator.ExpressionF.AddF
import net.andimiller.turtles.flying._

object Calculator {

  sealed trait Expression[T]
  object Expression {
    case class Add[T: Semigroup](a: Expression[T], b: Expression[T]) extends Expression[T] {
      def semigroup: Semigroup[T] = implicitly[Semigroup[T]]
    }
    case class IntLiteral(i: Int)       extends Expression[Int]
    case class StringLiteral(s: String) extends Expression[String]
  }

  sealed trait ExpressionF[+F[_], T]
  object ExpressionF {
    case class AddF[F[_], T: Semigroup](a: F[T], b: F[T]) extends ExpressionF[F, T] {
      def semigroup: Semigroup[T] = implicitly[Semigroup[T]]
    }
    case class IntLiteralF(i: Int)       extends ExpressionF[Nothing, Int]
    case class StringLiteralF(s: String) extends ExpressionF[Nothing, String]

    implicit val expressionfHFunctor: HFunctor[ExpressionF] = new HFunctor[ExpressionF] {
      def hmap[I[_], J[_]](nt: I ~> J): ExpressionF[I, *] ~> ExpressionF[J, *] = new (ExpressionF[I, *] ~> ExpressionF[J, *]) {
        override def apply[A](fa: ExpressionF[I, A]): ExpressionF[J, A] = fa match {
          case add: AddF[I, A] =>
            AddF(nt(add.a), nt(add.b))(add.semigroup)
          case IntLiteralF(i)    => IntLiteralF(i)
          case StringLiteralF(s) => StringLiteralF(s)
        }
      }
    }
  }

  import Expression._, ExpressionF._

  val unembed = new HAlgebra[ExpressionF, Expression] {
    def apply[A](fa: ExpressionF[Expression, A]): Expression[A] = fa match {
      case add: AddF[Expression, _] => Add(add.a, add.b)(add.semigroup)
      case IntLiteralF(i)           => IntLiteral(i)
      case StringLiteralF(s)        => StringLiteral(s)
    }
  }

  val embed = new HCoalgebra[ExpressionF, Expression] {
    def apply[A](fa: Expression[A]): ExpressionF[Expression, A] = fa match {
      case add @ Add(a, b)  => AddF(a, b)(add.semigroup)
      case IntLiteral(i)    => IntLiteralF(i)
      case StringLiteral(s) => StringLiteralF(s)
    }
  }

  val run: HAlgebra[ExpressionF, Id] = new HAlgebra[ExpressionF, Id] {
    def apply[A](fa: ExpressionF[Id, A]): Id[A] = fa match {
      case add @ AddF(a, b)  => add.semigroup.combine(a, b)
      case IntLiteralF(i)    => i
      case StringLiteralF(s) => s
    }
  }

  def main(args: Array[String]): Unit = {
    import net.andimiller.turtles.flying.schemes._
    // we can turn representations into each other
    val expr: Expression[Int] = Add(IntLiteral(1), IntLiteral(2))
    val embedder: Expression ~> HFix[ExpressionF, *] = hAna(embed)
    val exprf: HFix[ExpressionF, Int] = embedder(expr)
    println(expr) // Add(IntLiteral(1),IntLiteral(2))
    println(exprf) // HFix(AddF(HFix(IntLiteralF(1)),HFix(IntLiteralF(2))))

    // and we can run them
    val runner: HFix[ExpressionF, *] ~> Id = hCata(run)
    println(runner(exprf)) // 3
    println(
      runner(
        embedder(
          Add(
            StringLiteral("hello"),
            Add(
              StringLiteral(" "),
              StringLiteral("world")
            )
          )
        )
      )
    ) // "hello world"

    // and we can compose that
    val composed: Expression ~> Id = hhylo(run, embed)
    println(composed(Add(IntLiteral(2), IntLiteral(2)))) // 4
  }

}
