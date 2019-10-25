package net.andimiller.cli.parser

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import net.andimiller.cli.parser.Parser.Decoder
import net.andimiller.turtles.flying._

sealed trait Parser[+A]

object Parser {
  trait Decoder[I, O] {
    def decode(i: I): Either[String, O]
  }

  final case class Pure[A](a: A)    extends Parser[A]
  final case class Error(s: String) extends Parser[Nothing]

  final case class Map[I, O](p: Parser[I], f: I => O)               extends Parser[O]
  final case class EMap[I, O](p: Parser[I], decoder: Decoder[I, O]) extends Parser[O]
  final case class OrElse[O](a: Parser[O], b: Parser[O])            extends Parser[O]
  final case class Ap[A, B](ff: Parser[A => B], fa: Parser[A])      extends Parser[B]

  def orElse[O](a: Parser[O], b: Parser[O]): Parser[O] = OrElse(a, b)

  final case class HandleErrorWith[A](fa: Parser[A], f: String => Parser[A]) extends Parser[A]

  final case class CliArgument(placeholder: String)                     extends Parser[String]
  final case class CliOption(short: String, long: String, help: String) extends Parser[String]
  final case class Env(name: String, help: String)                      extends Parser[String]

  final case class Command[T](name: String, help: String, p: Parser[T]) extends Parser[T]

  final case class Many[T, O <: List[T]](p: Parser[T])          extends Parser[O]
  final case class Many1[T, O <: NonEmptyList[T]](p: Parser[T]) extends Parser[O]

  def optional[T](p: Parser[T]): Parser[Option[T]] =
    OrElse(Map(p, { t: T =>
      t.some
    }), Pure(None))
  def orFalse(p: Parser[Boolean]): Parser[Boolean]    = OrElse(p, Pure(false))
  def many[T](p: Parser[T]): Parser[List[T]]          = Many(p)
  def many1[T](p: Parser[T]): Parser[NonEmptyList[T]] = Many1(p)

  implicit val parserSemigroupal: Semigroupal[Parser] = new Semigroupal[Parser] {
    def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
      Ap(fa.map { a =>
        { (b: B) =>
          (a, b)
        }
      }, fb)
  }

  implicit val parserInstances: ApplicativeError[Parser, String] with Alternative[Parser] =
    new ApplicativeError[Parser, String] with Alternative[Parser] {
      def raiseError[A](e: String): Parser[A]                                  = Error(e)
      def handleErrorWith[A](fa: Parser[A])(f: String => Parser[A]): Parser[A] = HandleErrorWith(fa, f)
      def pure[A](x: A): Parser[A]                                             = Pure(x)
      def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B]               = Ap(ff, fa)
      def combineK[A](x: Parser[A], y: Parser[A]): Parser[A]                   = OrElse(x, y)
      def empty[A]: Parser[A]                                                  = Error("No parser supplied")
    }

}

sealed trait ParserF[+F[_], +A]

object ParserF {
  final case class PureF[A](a: A)    extends ParserF[Nothing, A]
  final case class ErrorF(s: String) extends ParserF[Nothing, Nothing]

  final case class MapF[F[_], I, O](p: F[I], f: I => O)                   extends ParserF[F, O]
  final case class EMapF[F[_], I, O](p: F[I], decoder: Decoder[I, O])     extends ParserF[F, O]
  final case class OrElseF[F[_], O](a: F[O], b: F[O])                     extends ParserF[F, O]
  final case class ApF[F[_], A, B](ff: F[A => B], fa: F[A])               extends ParserF[F, B]
  final case class HandleErrorWithF[F[_], A](fa: F[A], f: String => F[A]) extends ParserF[F, A]

  final case class CliArgumentF(placeholder: String)                      extends ParserF[Nothing, String]
  final case class CliOptionF(short: String, long: String, help: String)  extends ParserF[Nothing, String]
  final case class EnvF(name: String, help: String)                       extends ParserF[Nothing, String]
  final case class CommandF[F[_], T](name: String, help: String, p: F[T]) extends ParserF[F, T]

  final case class ManyF[F[_], T, O <: List[T]](p: F[T])          extends ParserF[F, O]
  final case class Many1F[F[_], T, O <: NonEmptyList[T]](p: F[T]) extends ParserF[F, O]

  implicit val parserHFunctor: HFunctor[ParserF] = new HFunctor[ParserF] {
    override def hmap[I[_], J[_]](nt: I ~> J): ParserF[I, *] ~> ParserF[J, *] = {
      new (ParserF[I, *] ~> ParserF[J, *]) {
        override def apply[A](fa: ParserF[I, A]): ParserF[J, A] = fa match {
          case PureF(a)                      => PureF(a)
          case ErrorF(s)                     => ErrorF(s)
          case mf: MapF[I, _, A]             => MapF(nt(mf.p), mf.f)
          case emf: EMapF[I, _, A]           => EMapF(nt(emf.p), emf.decoder)
          case oef: OrElseF[I, A]            => OrElseF(nt(oef.a), nt(oef.b))
          case af: ApF[I, _, A]              => ApF(nt(af.ff), nt(af.fa))
          case hewf: HandleErrorWithF[I, A]  => HandleErrorWithF(nt(hewf.fa), hewf.f.andThen(nt.apply))
          case CliArgumentF(placeholder)     => CliArgumentF(placeholder)
          case CliOptionF(short, long, help) => CliOptionF(short, long, help)
          case EnvF(name, help)              => EnvF(name, help)
          case cf: CommandF[I, A]            => CommandF(cf.name, cf.help, nt(cf.p))
          case mf: ManyF[I, _, A]            => ManyF(nt(mf.p))
          case m1f: Many1F[I, _, A]          => Many1F(nt(m1f.p))
        }
      }
    }

  }

}

object ParserAlgebras {
  val unfold: HCoalgebra[ParserF, Parser] = new HCoalgebra[ParserF, Parser] {
    override def apply[A](fa: Parser[A]): ParserF[Parser, A] = fa match {
      case Parser.Pure(a)                      => ParserF.PureF(a)
      case Parser.Error(s)                     => ParserF.ErrorF(s)
      case Parser.Map(p, f)                    => ParserF.MapF(p, f)
      case Parser.EMap(p, decoder)             => ParserF.EMapF(p, decoder)
      case Parser.OrElse(a, b)                 => ParserF.OrElseF(a, b)
      case Parser.Ap(ff, fa)                   => ParserF.ApF(ff, fa)
      case Parser.HandleErrorWith(fa, f)       => ParserF.HandleErrorWithF(fa, f)
      case Parser.CliArgument(placeholder)     => ParserF.CliArgumentF(placeholder)
      case Parser.CliOption(short, long, help) => ParserF.CliOptionF(short, long, help)
      case Parser.Env(name, help)              => ParserF.EnvF(name, help)
      case Parser.Command(name, help, p)       => ParserF.CommandF(name, help, p)
      case Parser.Many(p)                      => ParserF.ManyF(p)
      case Parser.Many1(p)                     => ParserF.Many1F(p)
    }
  }

  val fold: HAlgebra[ParserF, Parser] = new HAlgebra[ParserF, Parser] {
    override def apply[A](fa: ParserF[Parser, A]): Parser[A] = fa match {
      case p: ParserF.PureF[A]                   => Parser.Pure(p.a)
      case ParserF.ErrorF(s)                     => Parser.Error(s)
      case ParserF.MapF(p, f)                    => Parser.Map(p, f)
      case ParserF.EMapF(p, decoder)             => Parser.EMap(p, decoder)
      case ParserF.OrElseF(a, b)                 => Parser.OrElse(a, b)
      case ParserF.ApF(ff, fa)                   => Parser.Ap(ff, fa)
      case ParserF.HandleErrorWithF(fa, f)       => Parser.HandleErrorWith(fa, f)
      case clif: ParserF.CliArgumentF            => Parser.CliArgument(clif.placeholder)
      case ParserF.CliOptionF(short, long, help) => Parser.CliOption(short, long, help)
      case ParserF.EnvF(name, help)              => Parser.Env(name, help)
      case ParserF.CommandF(name, help, p)       => Parser.Command(name, help, p)
      case ParserF.ManyF(p)                      => Parser.Many(p)
      case ParserF.Many1F(p)                     => Parser.Many1(p)
    }
  }
}
