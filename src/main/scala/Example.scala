import HZoo._
import cats._
import cats.implicits._
import cats.data._
import Parser._
import ParserF._

object Example {

  case class RunnableParser[T](run: StateT[EitherT[Eval, String, *], String, T])

  object RunnableParser {
    val underlying: ApplicativeError[StateT[EitherT[Eval, String, *], String, *], String] = implicitly

    def fromStateT[A](s: StateT[Either[String, *], String, A]): RunnableParser[A] =
      RunnableParser(s.mapK(new (Either[String, *] ~> EitherT[Eval, String, *]) {
        override def apply[A](fa: Either[String, A]): EitherT[Eval, String, A] =
          EitherT(Eval.now(fa))
      }))

    implicit val runnableParserApplicative: Applicative[RunnableParser] = new Applicative[RunnableParser] {
      def pure[A](x: A): RunnableParser[A] = RunnableParser(underlying.pure(x))
      def ap[A, B](ff: RunnableParser[A => B])(fa: RunnableParser[A]): RunnableParser[B] =
        RunnableParser(underlying.ap(ff.run)(fa.run))
    }
  }

  val run: HAlgebra[ParserF, RunnableParser] = new HAlgebra[ParserF, RunnableParser] {
    override def apply[A](fa: ParserF[RunnableParser, A]): RunnableParser[A] = fa match {
      case ParserF.PureF(a) =>
        RunnableParser(RunnableParser.underlying.pure(a))
      case ParserF.ErrorF(s) =>
        RunnableParser(RunnableParser.underlying.raiseError(s))
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
      case oef: OrElseF[RunnableParser, A] =>
        RunnableParser(oef.a.run.orElse(oef.b.run))
      case apf: ApF[RunnableParser, _, A] =>
        RunnableParser(apf.ff.run.ap(apf.fa.run))
      case rwf: RecoverWithF[RunnableParser, A] =>
        RunnableParser(rwf.fa.run.handleErrorWith(rwf.f.andThen(_.run)))
      case ncf: NoConsumeF[RunnableParser, A] =>
        RunnableParser(for {
          s <- StateT.get[EitherT[Eval, String, *], String]
          r <- ncf.p.run
          _ <- StateT.set[EitherT[Eval, String, *], String](s)
        } yield r)
    }
  }

  def main(args: Array[String]): Unit = {

    def runner[O](p: HFix[ParserF, O]): String => Either[String, (String, O)] = {
      val program = hCata(run, p).run

      { s: String =>
        program.run(s).value.value
      }
    }

    def project[O](p: HFix[ParserF, O]): Parser[O] = hCata(ParserAlgebras.fold, p)
    def embed[O](p: Parser[O]): HFix[ParserF, O]   = hAna(ParserAlgebras.unfold, p)

    val program1 = Map(
      Literal('a'), { c: Char =>
        s"I parsed the character $c"
      }
    )

    println(runner(embed(program1))("abc"))

    val program2 = Parser.FlatMap(
      Literals("abc"), { a: String =>
        Map(
          Literals("def"), { b: String =>
            s"I parsed $a then $b"
          }
        )
      }
    )

    println(runner(embed(program2))("abcdefghi"))

    val program3 = {
      import Parser._
      val greeting = Literals("hello").orElse(Literals("hi"))
      val space    = Literal(' ')
      val world    = Literals("world")

      (greeting, space, world).tupled
    }

    println(runner(embed(program3))("hello world"))
    println(runner(embed(program3))("hi world"))
    println(runner(embed(program3))("hi"))

    val program4 = {
      import Parser._
      for {
        a          <- Pure("pure value")
        firstWord  <- TakeWhile(_ != ' ')
        _          <- Literal(' ')
        secondWord <- TakeWhile(_ != ' ')
      } yield (a, firstWord, secondWord)
    }

    val compiledProgram4 = runner(embed(program4))

    println(compiledProgram4("hello world"))
    println(compiledProgram4("recursion schemes are cool"))
    println(compiledProgram4("123 456 789"))
    println(compiledProgram4("123 "))
    println(compiledProgram4("123"))

    val program5 = {
      import Parser._
      val a = Literal('a')
      rep(a)
    }

    val compiledProgram5 = runner(embed(program5))
    println(compiledProgram5("aaaabcd"))

    /*
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

   */

  }
}
