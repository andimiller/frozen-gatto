import cats._
import cats.implicits._
import cats.effect._
import cats.data._
import higherkindness.droste.data.Fix

sealed trait Parser[A] {
  def map[B](f: A => B): Parser[B]  = Parser.Map(this, f)
  def flatMap[B](f: A => Parser[B]) = Parser.FlatMap(this, f)
}

object Parser {
  case class Literal(c: Char)              extends Parser[Char]
  case class Literals(s: String)           extends Parser[String]
  case class TakeWhile(f: Char => Boolean) extends Parser[String]

  case class Map[I, O](p: Parser[I], f: I => O)             extends Parser[O]
  case class FlatMap[I, O](p: Parser[I], f: I => Parser[O]) extends Parser[O]
}

sealed trait ParserF[+F[_], A]

object ParserF {
  final case class LiteralF(c: Char)              extends ParserF[Nothing, Char]
  final case class LiteralsF(s: String)           extends ParserF[Nothing, String]
  final case class TakeWhileF(f: Char => Boolean) extends ParserF[Nothing, String]

  final case class MapF[F[_], I, O](p: F[I], f: I => O)        extends ParserF[F, O]
  final case class FlatMapF[F[_], I, O](p: F[I], f: I => F[O]) extends ParserF[F, O]

  implicit val parserHFunctor: HFunctor[ParserF] = new HFunctor[ParserF] {
    override def hmap[I[_], J[_]](nt: I ~> J): ParserF[I, *] ~> ParserF[J, *] = {
      new (ParserF[I, *] ~> ParserF[J, *]) {
        override def apply[A](fa: ParserF[I, A]): ParserF[J, A] = fa match {
          case LiteralF(c)                 => LiteralF(c)
          case LiteralsF(s)                => LiteralsF(s)
          case TakeWhileF(f)               => TakeWhileF(f)
          case mapf: MapF[I, _, A]         => MapF(nt(mapf.p), mapf.f)
          case flatmapf: FlatMapF[I, _, A] => FlatMapF(nt(flatmapf.p), Kleisli(flatmapf.f).mapK(nt).run)
        }
      }
    }

  }

}
