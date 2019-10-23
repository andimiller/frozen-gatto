package net.andimiller.turtles.flying

import cats.~>

package object schemes {

  def hCata[F[_[_], _], G[_]](alg: HAlgebra[F, G])(implicit F: HFunctor[F]): HFix[F, *] ~> G =
    new (HFix[F, *] ~> G) {
      def apply[A](fa: HFix[F, A]): G[A] = {
        val inner = fa.unfix
        val nt = F.hmap(new (HFix[F, *] ~> G) {
          def apply[A](fa: HFix[F, A]): G[A] =
            hCata(alg).apply(fa)
        })(inner)
        alg(nt)
      }
    }

  def hAna[F[_[_], _], G[_]](coalg: HCoalgebra[F, G])(implicit F: HFunctor[F]): G ~> HFix[F, *] =
    new (G ~> HFix[F, *]) {
      def apply[A](fa: G[A]): HFix[F, A] = {
        val nt = F.hmap(new (G ~> HFix[F, *]) {
          def apply[A](fa: G[A]): HFix[F, A] =
            hAna(coalg).apply(fa)
        })
        HFix(nt(coalg(fa)))
      }
    }

  def hhylo[F[_[_], _], A[_], B[_]](
      halg: HAlgebra[F, B],
      hcoalg: HCoalgebra[F, A]
  )(implicit F: HFunctor[F]): A ~> B = new (A ~> B) {
    def apply[T](fa: A[T]): B[T] = {
      val nt = F.hmap(hhylo(halg, hcoalg))
      halg(nt(hcoalg(fa)))
    }
  }

}
