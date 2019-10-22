import HZoo.{HAlgebra, HCoalgebra}
import cats._
import cats.implicits._
import cats.effect._
import cats.data._

sealed trait Parser[A] {
  def map[B](f: A => B): Parser[B]        = Parser.Map(this, f)
  def flatMap[B](f: A => Parser[B])       = Parser.FlatMap(this, f)
  def orElse(other: Parser[A]): Parser[A] = Parser.OrElse(this, other)
}

object Parser {
  final case class Pure[A](a: A)                 extends Parser[A]
  final case class Error(s: String)              extends Parser[Nothing]
  final case class Literal(c: Char)              extends Parser[Char]
  final case class Literals(s: String)           extends Parser[String]
  final case class TakeWhile(f: Char => Boolean) extends Parser[String]

  final case class Map[I, O](p: Parser[I], f: I => O)             extends Parser[O]
  final case class FlatMap[I, O](p: Parser[I], f: I => Parser[O]) extends Parser[O]
  final case class OrElse[O](a: Parser[O], b: Parser[O])          extends Parser[O]
  final case class Ap[A, B](ff: Parser[A => B], fa: Parser[A])    extends Parser[B]

  def opt[A](p: Parser[A]): Parser[Option[A]] = RecoverWith(p.map(_.some), _ => Pure(none))
  def rep[A](p: Parser[A]): Parser[List[A]]   = p.whileM(NoConsume(p).asInstanceOf[Parser[A]].attempt.map(_.isRight))

  final case class RecoverWith[A](fa: Parser[A], f: String => Parser[A]) extends Parser[A]
  final case class NoConsume[O](p: Parser[O])                            extends Parser[O]

  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    def pure[A](x: A): Parser[A]                               = Pure(x)
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Ap(ff, fa)
  }

  implicit val parserMonad: Monad[Parser] = new StackSafeMonad[Parser] {
    def pure[A](x: A): Parser[A] = Pure(x)
    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      FlatMap(fa, f)
  }

  implicit val parserInvariant: Invariant[Parser] = new Invariant[Parser] {
    def imap[A, B](fa: Parser[A])(f: A => B)(g: B => A): Parser[B] =
      fa.map(f)
  }

  implicit val parserSemigroupal: Semigroupal[Parser] = new Semigroupal[Parser] {
    def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
      Ap(fa.map { a =>
        { (b: B) =>
          (a, b)
        }
      }, fb)
  }

  implicit val parserMonadError: MonadError[Parser, String] = new MonadError[Parser, String] with StackSafeMonad[Parser] {
    def raiseError[A](e: String): Parser[A]                                  = Error(e).asInstanceOf[Parser[A]]
    def handleErrorWith[A](fa: Parser[A])(f: String => Parser[A]): Parser[A] = RecoverWith(fa, f)
    def pure[A](x: A): Parser[A]                                             = Pure(x)
    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B]           = FlatMap(fa, f)
  }

}

sealed trait ParserF[+F[_], A]

object ParserF {
  final case class PureF[A](a: A)                 extends ParserF[Nothing, A]
  final case class ErrorF(s: String)              extends ParserF[Nothing, Nothing]
  final case class LiteralF(c: Char)              extends ParserF[Nothing, Char]
  final case class LiteralsF(s: String)           extends ParserF[Nothing, String]
  final case class TakeWhileF(f: Char => Boolean) extends ParserF[Nothing, String]

  final case class MapF[F[_], I, O](p: F[I], f: I => O)        extends ParserF[F, O]
  final case class FlatMapF[F[_], I, O](p: F[I], f: I => F[O]) extends ParserF[F, O]
  final case class OrElseF[F[_], O](a: F[O], b: F[O])          extends ParserF[F, O]
  final case class ApF[F[_], A, B](ff: F[A => B], fa: F[A])    extends ParserF[F, B]

  final case class RecoverWithF[F[_], A](fa: F[A], f: String => F[A]) extends ParserF[F, A]
  final case class NoConsumeF[F[_], O](p: F[O])                       extends ParserF[F, O]

  implicit val parserHFunctor: HFunctor[ParserF] = new HFunctor[ParserF] {
    override def hmap[I[_], J[_]](nt: I ~> J): ParserF[I, *] ~> ParserF[J, *] = {
      new (ParserF[I, *] ~> ParserF[J, *]) {
        override def apply[A](fa: ParserF[I, A]): ParserF[J, A] = fa match {
          case PureF(a)                    => PureF(a)
          case ErrorF(s)                   => ErrorF(s)
          case LiteralF(c)                 => LiteralF(c)
          case LiteralsF(s)                => LiteralsF(s)
          case TakeWhileF(f)               => TakeWhileF(f)
          case mapf: MapF[I, _, A]         => MapF(nt(mapf.p), mapf.f)
          case flatmapf: FlatMapF[I, _, A] => FlatMapF(nt(flatmapf.p), flatmapf.f.andThen(nt.apply))
          case orelsef: OrElseF[I, A]      => OrElseF(nt(orelsef.a), nt(orelsef.b))
          case apf: ApF[I, _, A]           => ApF(nt(apf.ff), nt(apf.fa))
          case rwf: RecoverWithF[I, A]     => RecoverWithF(nt(rwf.fa), rwf.f.andThen(nt.apply))
          case ncf: NoConsumeF[I, A]       => NoConsumeF(nt(ncf.p))
        }
      }
    }

  }

}

object ParserAlgebras {
  val unfold: HCoalgebra[ParserF, Parser] = new HCoalgebra[ParserF, Parser] {
    override def apply[A](fa: Parser[A]): ParserF[Parser, A] = fa match {
      case Parser.Pure(a)            => ParserF.PureF(a)
      case Parser.Error(s)           => ParserF.ErrorF(s)
      case Parser.Literal(c)         => ParserF.LiteralF(c)
      case Parser.Literals(s)        => ParserF.LiteralsF(s)
      case Parser.TakeWhile(f)       => ParserF.TakeWhileF(f)
      case Parser.Map(p, f)          => ParserF.MapF(p, f)
      case Parser.FlatMap(p, f)      => ParserF.FlatMapF(p, f)
      case Parser.OrElse(a, b)       => ParserF.OrElseF(a, b)
      case Parser.Ap(ff, fa)         => ParserF.ApF(ff, fa)
      case Parser.RecoverWith(fa, f) => ParserF.RecoverWithF(fa, f)
      case Parser.NoConsume(p)       => ParserF.NoConsumeF(p)
    }
  }

  val fold: HAlgebra[ParserF, Parser] = new HAlgebra[ParserF, Parser] {
    override def apply[A](fa: ParserF[Parser, A]): Parser[A] = fa match {
      case ParserF.PureF(a)            => Parser.Pure(a)
      case ParserF.ErrorF(s)           => Parser.Error(s)
      case ParserF.LiteralF(c)         => Parser.Literal(c)
      case ParserF.LiteralsF(s)        => Parser.Literals(s)
      case ParserF.TakeWhileF(f)       => Parser.TakeWhile(f)
      case ParserF.MapF(p, f)          => Parser.Map(p, f)
      case ParserF.FlatMapF(p, f)      => Parser.FlatMap(p, f)
      case ParserF.OrElseF(a, b)       => Parser.OrElse(a, b)
      case ParserF.ApF(ff, fa)         => Parser.Ap(ff, fa)
      case ParserF.RecoverWithF(fa, f) => Parser.RecoverWith(fa, f)
      case ParserF.NoConsumeF(p)       => Parser.NoConsume(p)
    }
  }
}
