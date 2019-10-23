package net.andimiller.turtles.flying

import cats.~>

trait HFunctor[F[_[_], _]] {
  def hmap[I[_], J[_]](nt: I ~> J): F[I, *] ~> F[J, *]
}
