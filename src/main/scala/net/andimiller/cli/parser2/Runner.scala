package net.andimiller.cli.parser2

import Parser._
import cats._
import cats.data.{EitherT, State, StateT}
import cats.implicits._
import net.andimiller.turtles.flying.HAlgebra

object Runner {

  case class ParserInput(args: List[String], env: Map[String, String] = Map.empty)

  def popHead(metavar: String): EitherT[StateT[Eval, ParserInput, *], String, String] = EitherT(
    StateT { s =>
      s.args match {
        case head :: tail => Eval.now(head.asRight[String]).tupleLeft(ParserInput(tail, s.env))
        case _            => Eval.now(s"Argument expected: $metavar".asLeft[String]).tupleLeft(s)
      }
    }
  )

  implicit class IndexOps(i: Int) {
    def asIndex: Option[Int] = if (i == -1) None else Some(i)
  }

  def popArg(long: String, short: Option[String]): EitherT[StateT[Eval, ParserInput, *], String, String] = EitherT(
    StateT { s =>
      val foundAt: Option[Int] = s.args.indexOf(long).asIndex.orElse(short.flatMap(sh => s.args.indexOf(sh).asIndex))
      foundAt match {
        case Some(i) =>
          s.args.get(i + 1) match {
            case Some(value) =>
              Eval.now(value.asRight[String]).tupleLeft(ParserInput(s.args.drop(i).drop(i), s.env))
            case None =>
              Eval.now(s"Expected a value for $long".asLeft[String]).tupleLeft(s)
          }
        case None =>
          Eval.now(s"Expected $long or $short".asLeft[String]).tupleLeft(s)
      }
    }
  )

  def getEnv(name: String): EitherT[StateT[Eval, ParserInput, *], String, String] = EitherT(
    StateT { s =>
      s.env.get(name) match {
        case Some(value) =>
          Eval.now(value.asRight[String]).tupleLeft(s)
        case None =>
          Eval.now(s"Expected environment variable $name".asLeft[String]).tupleLeft(s)
      }
    }
  )

  type ParserRun[O] = EitherT[StateT[Eval, ParserInput, *], String, O]

  implicit class CastSyntax[F[_]: Functor, I](f: F[I]) {
    def iKnowItsAn[O]: F[O] = f.map(_.asInstanceOf[O])
  }

  val parserRunner = new HAlgebra[ParserF, ParserRun] {
    override def apply[A](fa: ParserF[ParserRun, A]): ParserRun[A] = fa match {
      case ParserF.PureF(a) =>
        EitherT(StateT { s =>
          Eval.now((s, a.asRight[String]))
        })
      case ParserF.EmptyF =>
        EitherT(StateT { s =>
          Eval.now(s, "Empty value".asLeft)
        })
      case mf: ParserF.MapF[ParserRun, _, A] => mf.p.map(mf.f)
      case emf: ParserF.EMapF[ParserRun, _, A] =>
        emf.p.flatMap(o =>
          EitherT(StateT { s =>
            Eval.now((s, emf.decoder(o)))
          }))
      case oef: ParserF.OrElseF[ParserRun, A]  => oef.a.orElse(oef.b)
      case ParserF.ApF(ff, fa)   => ff.ap(fa)
      case af: ParserF.ArgF      => popHead(af.metavar).iKnowItsAn[A]
      case lof: ParserF.LongOptF => popArg(lof.long, lof.short).iKnowItsAn[A]
      case ef: ParserF.EnvF      => getEnv(ef.name).iKnowItsAn[A]
    }
  }

  def main(args: Array[String]): Unit = {
    import Parser._
    import net.andimiller.turtles.flying.schemes._
    val runner = hhylo(parserRunner, embed)
    def run[O](s: ParserInput)(p: Parser[O]): Either[String, O] = runner(p).value.run(s).value._2
    println(run(ParserInput(List.empty))(
      Pure("hello world")
    ))

    println(run(
      ParserInput(List.empty, scala.Predef.Map("NUMBER" -> "123"))
    )(
      Env("NUMBER", "a number").map(_.toInt)
    ))

    println(run(
      ParserInput(List.empty, scala.Predef.Map())
    )(
      Env("NUMBER", "a number").map(_.toInt)
    ))
  }

}
