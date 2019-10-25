package net.andimiller.cli.parser

import cats._
import cats.data.NonEmptyList
import net.andimiller.turtles.flying.HAlgebra
import net.andimiller.turtles.flying._
import net.andimiller.turtles.flying.schemes._

object HelpDialogue {

  sealed trait Help[+A]
  case object Blank extends Help[Nothing]
  case class OptionHelp[A](short: String, long: String, help: String)
  case class EnvHelp[A](name: String, help: String)
  case class CommandHelp[A](command: String, options: List[OptionHelp[A]], envs: List[EnvHelp[A]], subcommands: List[CommandHelp[A]]) extends Help[A]

  object CommandHelp {
    implicit def semigroup[A]: Semigroup[CommandHelp[A]] = new Semigroup[CommandHelp[A]] {
      def combine(x: CommandHelp[A], y: CommandHelp[A]): CommandHelp[A] =
        CommandHelp(
          x.command + " " + y.command,
          x.options ++ y.options,
          x.envs ++ y.envs,
          x.subcommands ++ y.subcommands
        )
    }
  }

  object Help {
    implicit def semigrou[A]: Semigroup[Help[A]] = new Semigroup[Help[A]] {

    }
  }


  type HelpBuilder[A] = Help[A]

  val printer: HAlgebra[ParserF, HelpBuilder] = new HAlgebra[ParserF, HelpBuilder] {
    def apply[A](fa: ParserF[HelpBuilder, A]): HelpBuilder[A] = fa match {
      case ParserF.PureF(a)                      => Blank
      case ParserF.ErrorF(s)                     => Blank
      case ParserF.MapF(p, f)                    => p
      case ParserF.EMapF(p, decoder)             => p
      case ParserF.OrElseF(a, b)                 =>
      case ParserF.ApF(ff, fa)                   => NonEmptyList(ff, fa)  s"$ff <> $fa"
      case ParserF.HandleErrorWithF(fa, f)       => fa
      case ParserF.CliArgumentF(placeholder)     => s"<$placeholder>"
      case ParserF.CliOptionF(short, long, help) => s"-$short, --$long $help"
      case ParserF.EnvF(name, help)              => s"$name $help"
      case ParserF.CommandF(name, help, p)       => s"$name $help $p"
      case ParserF.ManyF(p)                      => s"$p..."
      case ParserF.Many1F(p)                     => s"$p $p..."
    }
  }

  def main(args: Array[String]): Unit = {
    //val help = hhylo(generator, ParserAlgebras.unfold)

    import Parser._
    import cats.implicits._

    val lsOptions: Parser[(Boolean, Boolean)] = (
      CliOption(
        "a",
        "all",
        "do not ignore entries starting with ."
      ).map(_ => true).orElse(Pure(false)),
      CliOption(
        "A",
        "almost-all",
        "do not list implied . and .."
      ).map(_ => true).orElse(Pure(false)),
    ).tupled

    println(Command("ls", "list files", lsOptions))
//    println(help(Command("ls", "list files", lsOptions)))
  }

}
