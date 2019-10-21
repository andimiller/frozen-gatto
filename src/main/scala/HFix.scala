import cats.~>
import higherkindness.droste.{Algebra, Coalgebra}
import higherkindness.droste.data.Fix
import higherkindness.droste.meta.Meta

case class HFix[F[_[_], _], A](unfix: F[HFix[F, *], A])

trait HFunctor[F[_[_], _]] {
  def hmap[I[_], J[_]](nt: I ~> J): F[I, *] ~> F[J, *]
}

object HZoo {
  type HAlgebra[F[_[_], _], G[_]] = F[G, *] ~> G
  def hCata[F[_[_], _], G[_], I](alg: HAlgebra[F, G], hfix: HFix[F, I])(implicit F: HFunctor[F]): G[I] = {
    val inner = hfix.unfix
    val nt = F.hmap(
      new (HFix[F, *] ~> G) {
        def apply[A](fa: HFix[F, A]): G[A] = hCata(alg, fa)
      }
    )(inner)
    alg(nt)
  }
}
