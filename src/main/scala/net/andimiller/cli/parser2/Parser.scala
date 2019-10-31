package net.andimiller.cli.parser2

import cats._
import cats.arrow.Profunctor
import cats.implicits._
import cats.data._
import net.andimiller.turtles.flying.{HAlgebra, HCoalgebra, HFunctor}

import scala.language.higherKinds

object Parser {

  trait Decoder[I, O] {
    def apply(i: I): Either[String, O]
  }

  object Decoder {
    implicit def decoderProfunctor: Profunctor[Decoder] = new Profunctor[Decoder] {
      override def dimap[A, B, C, D](fab: Decoder[A, B])(f: C => A)(g: B => D): Decoder[C, D] =
        (i: C) => fab(f(i)).map(g)
    }
    implicit def decoderContravariant[O]: Contravariant[Decoder[*, O]] = new Contravariant[Decoder[*, O]] {
      override def contramap[A, B](fa: Decoder[A, O])(f: B => A): Decoder[B, O] = (i: B) => fa(f(i))
    }
    implicit def decoderFunctor[I]: Functor[Decoder[I, *]] = new Functor[Decoder[I, *]] {
      override def map[A, B](fa: Decoder[I, A])(f: A => B): Decoder[I, B] = (i: I) => fa(i).map(f)
    }
  }

  sealed trait Parser[+O]
  object Parser {
    final case class Pure[A](a: A) extends Parser[A]
    final case object Empty        extends Parser[Nothing]

    final case class Map[I, O](p: Parser[I], f: I => O)               extends Parser[O]
    final case class EMap[I, O](p: Parser[I], decoder: Decoder[I, O]) extends Parser[O]
    final case class OrElse[O](a: Parser[O], b: Parser[O])            extends Parser[O]
    final case class Ap[A, B](ff: Parser[A => B], fa: Parser[A])      extends Parser[B]

    final case class Arg(metavar: String) extends Parser[String]
    final case class LongOpt(long: String, short: Option[String], metavar: Option[String], default: Option[String], help: Option[String])
        extends Parser[String]
    final case class Env(name: String, help: String) extends Parser[String]

    implicit val instances: Applicative[Parser] with Alternative[Parser] = new Applicative[Parser] with Alternative[Parser] {
      override def pure[A](x: A): Parser[A]                               = Pure(x)
      override def empty[A]: Parser[A]                                    = Empty
      override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A]     = OrElse(x, y)
      override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Ap(ff, fa)
    }
  }

  sealed trait ParserF[+F[_], +O]
  object ParserF {
    final case class PureF[A](a: A) extends ParserF[Nothing, A]
    final case object EmptyF        extends ParserF[Nothing, Nothing]

    final case class MapF[F[_], I, O](p: F[I], f: I => O)               extends ParserF[F, O]
    final case class EMapF[F[_], I, O](p: F[I], decoder: Decoder[I, O]) extends ParserF[F, O]
    final case class OrElseF[F[_], O](a: F[O], b: F[O])                 extends ParserF[F, O]
    final case class ApF[F[_], A, B](ff: F[A => B], fa: F[A])           extends ParserF[F, B]

    final case class ArgF(metavar: String) extends ParserF[Nothing, String]
    final case class LongOptF(long: String, short: Option[String], metavar: Option[String], default: Option[String], help: Option[String])
        extends ParserF[Nothing, String]
    final case class EnvF(name: String, help: String) extends ParserF[Nothing, String]

    implicit val parserFHFunctor: HFunctor[ParserF] = new HFunctor[ParserF] {
      override def hmap[I[_], J[_]](nt: I ~> J): ParserF[I, *] ~> ParserF[J, *] =
        new (ParserF[I, *] ~> ParserF[J, *]) {
          override def apply[A](fa: ParserF[I, A]): ParserF[J, A] = fa match {
            case PureF(a)                                      => PureF(a)
            case EmptyF                                        => EmptyF
            case mf: MapF[I, _, A]                             => MapF(nt(mf.p), mf.f)
            case emf: EMapF[I, _, A]                           => EMapF(nt(emf.p), emf.decoder)
            case oef: OrElseF[I, A]                            => OrElseF(nt(oef.a), nt(oef.b))
            case af: ApF[I, _, A]                              => ApF(nt(af.ff), nt(af.fa))
            case ArgF(metavar)                                 => ArgF(metavar)
            case LongOptF(long, short, metavar, default, help) => LongOptF(long, short, metavar, default, help)
            case EnvF(name, help)                              => EnvF(name, help)
          }
        }
    }
  }

  val project: HAlgebra[ParserF, Parser] = new HAlgebra[ParserF, Parser] {
    override def apply[A](fa: ParserF[Parser, A]): Parser[A] = fa match {
      case ParserF.PureF(a)                                      => Parser.Pure(a)
      case ParserF.EmptyF                                        => Parser.Empty
      case ParserF.MapF(p, f)                                    => Parser.Map(p, f)
      case ParserF.EMapF(p, decoder)                             => Parser.EMap(p, decoder)
      case ParserF.OrElseF(a, b)                                 => Parser.OrElse(a, b)
      case ParserF.ApF(ff, fa)                                   => Parser.Ap(ff, fa)
      case ParserF.ArgF(metavar)                                 => Parser.Arg(metavar)
      case ParserF.LongOptF(long, short, metavar, default, help) => Parser.LongOpt(long, short, metavar, default, help)
      case ParserF.EnvF(name, help)                              => Parser.Env(name, help)
    }
  }

  val embed: HCoalgebra[ParserF, Parser] = new HCoalgebra[ParserF, Parser] {
    override def apply[A](fa: Parser[A]): ParserF[Parser, A] = fa match {
      case Parser.Pure(a)                                      => ParserF.PureF(a)
      case Parser.Empty                                        => ParserF.EmptyF
      case Parser.Map(p, f)                                    => ParserF.MapF(p, f)
      case Parser.EMap(p, decoder)                             => ParserF.EMapF(p, decoder)
      case Parser.OrElse(a, b)                                 => ParserF.OrElseF(a, b)
      case Parser.Ap(ff, fa)                                   => ParserF.ApF(ff, fa)
      case Parser.Arg(metavar)                                 => ParserF.ArgF(metavar)
      case Parser.LongOpt(long, short, metavar, default, help) => ParserF.LongOptF(long, short, metavar, default, help)
      case Parser.Env(name, help)                              => ParserF.EnvF(name, help)
    }
  }

  // add me later
  final case class Command[T](name: String, help: Option[String], p: Parser[T])
  object Command {}

}
