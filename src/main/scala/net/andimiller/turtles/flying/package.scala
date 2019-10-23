package net.andimiller.turtles

import cats.~>

package object flying {
  case class HFix[F[_[_], _], A](unfix: F[HFix[F, *], A])

  // algebra types
  type HAlgebra[F[_[_], _], G[_]]   = F[G, *] ~> G
  type HCoalgebra[F[_[_], _], G[_]] = G ~> F[G, *]
}
