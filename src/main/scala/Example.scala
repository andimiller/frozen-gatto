import HZoo._
import Parser._
import ParserF._
import cats._, cats.implicits._
import cats.data._

object Example {

  case class RunnableParser[T](run: StateT[EitherT[Eval, String, *], String, T])

  object RunnableParser {
    def fromStateT[A](s: StateT[Either[String, *], String, A]): RunnableParser[A] =
      RunnableParser(s.mapK(new (Either[String, *] ~> EitherT[Eval, String, *]) {
        override def apply[A](fa: Either[String, A]): EitherT[Eval, String, A] =
          EitherT(Eval.now(fa))
      }))
  }

  val run: HAlgebra[ParserF, RunnableParser] = new HAlgebra[ParserF, RunnableParser] {
    override def apply[A](fa: ParserF[RunnableParser, A]): RunnableParser[A] = fa match {
      case ParserF.LiteralF(c) =>
        RunnableParser.fromStateT(StateT { s: String =>
          if (s.headOption == Option(c))
            (s.tail, c).asRight[String].asInstanceOf[Either[String, (String, A)]]
          else
            s"Expected $c".asLeft[(String, A)]
        })
      case ParserF.LiteralsF(target) =>
        RunnableParser.fromStateT(StateT { s: String =>
          if (s.startsWith(target))
            s.splitAt(target.length).swap.asRight[String].asInstanceOf[Either[String, (String, A)]]
          else
            s"Expected $target".asLeft[(String, A)]
        })
      case TakeWhileF(f) =>
        RunnableParser.fromStateT(StateT { s: String =>
          val result = s.takeWhile(f)
          (s.drop(result.length), result).asRight[String].asInstanceOf[Either[String, (String, A)]]
        })
      case mf: MapF[RunnableParser, _, A] =>
        RunnableParser(mf.p.run.map(mf.f))
      case fmf: FlatMapF[RunnableParser, _, A] =>
        RunnableParser(fmf.p.run.flatMap(r => fmf.f(r).run))
    }
  }

  def main(args: Array[String]): Unit = {

    val r1 = hCata(run, HFix(LiteralF('a'): ParserF[HFix[ParserF, *], Char])).run.run("abc").value.value
    val r2 = hCata(run,
                   HFix(
                     MapF(
                       HFix(LiteralsF("abc"): ParserF[HFix[ParserF, *], String]),
                       (s: String) => s"I just parsed $s"
                     )
                   )).run.run("abcdef").value.value

    println(r1, r2)

    //MapF(HFix(TakeWhileF(_ != '\n')), s => s"I got the string: $s")

  }
}
