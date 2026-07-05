package io.forward.polyfunctors

case class Tri[+A, +B, +C](a: A, b: B, c: C)

trait Trifunctor[F[+_, +_, +_]] {
  def trimap[A, B, C, X, Y, Z](domain: F[A, B, C], f: A => X, g: B => Y, h: C => Z): F[X, Y, Z]
}

object Trifunctor {
  def apply[F[+_, +_, +_]](implicit F: Trifunctor[F]): Trifunctor[F] = F
  implicit def Tuple3Trifunctor: Trifunctor[Tuple3] = new Trifunctor[Tuple3] {
    def trimap[A, B, C, X, Y, Z](k: (A, B, C), f: A => X, g: B => Y, h: C => Z) =
      (f(k._1), g(k._2), h(k._3))
  }
}
