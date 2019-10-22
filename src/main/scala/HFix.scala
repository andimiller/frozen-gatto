import cats.~>

import scala.language.higherKinds

case class HFix[F[_[_], _], A](unfix: F[HFix[F, *], A])

trait HFunctor[F[_[_], _]] {
  def hmap[I[_], J[_]](nt: I ~> J): F[I, *] ~> F[J, *]
}

object HZoo {

  // Algebra[F[_], A] = F[A] => A
  // def cata[F[_]: Functor, R, B](algebra: Algebra[F, B](implicit project: Project[F, R]): R => B
  // cata :: Functor f => Algebra f a -> FixF f -> a
  // hcata :: HFunctor f => HAlgebra f a -> FixH f :~> a
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

  // Coalgebra[F[_], A] = A => F[A]
  // def ana[F[_]: Functor, A, R](coalgebra: Coalgebra[F, A])(implicit embed: Embed[F, R]): A => R)
  // ana :: Functor f => Coalgebra f a -> a -> FixF f
  // hana :: HFunctor f => HCoalgebra f a -> a :~> FixH f
  type HCoalgebra[F[_[_], _], G[_]] = G ~> F[G, *]
  def hAna[F[_[_], _], G[_], I](coalg: HCoalgebra[F, G], fi: G[I])(implicit F: HFunctor[F]): HFix[F, I] = {
    val nt = F.hmap(
      new (G ~> HFix[F, *]) {
        override def apply[A](fa: G[A]): HFix[F, A] =
          hAna(coalg, fa)
      }
    )
    HFix(nt(coalg(fi)))
  }
}
