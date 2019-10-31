import org.scalameter.api._
import net.andimiller.turtles.flying._
import net.andimiller.turtles.flying.schemes._
import net.andimiller.gatto.frozen.Parser._
import net.andimiller.gatto.frozen.Example._
import cats.implicits._
import net.andimiller.gatto.frozen.{Parser, ParserF}
import org.scalameter.picklers.noPickler._

object ParserBench extends Bench.LocalTime {

  val program = (
    Literal('a'),
    Literal('b'),
    Literal('c')
  ).tupled

  val embedded = embed(program)

  val compiled = runner(embedded)

  val noop = Gen.unit("nah")

  val programs: Gen[Parser[Unit]] = Gen.enumeration("program")(
      Pure(()),
      (Literal('a'), Literal('b'), Literal('b')).tupled.map(_ => ()),
      (Literals("abc").map(_ => ()))
    )

  val embeddeds: Gen[HFix[ParserF, Unit]] = Gen.enumeration("embedded")(
    embed(Pure(())),
    embed((Literal('a'), Literal('b'), Literal('b')).tupled.map(_ => ())),
    embed((Literals("abc").map(_ => ())))
  )
  performance of "Parser" in {
    measure method "embed" in {
      using(programs) in { program =>
        embed(program)
      }
    }
    measure method "compile" in {
      using (embeddeds) in { embedded =>
        runner(embedded)
      }
    }
    measure method "run" in {
      using (noop) in { _ =>
        compiled("abc")
      }
    }
  }

}
