package io.forward.shapes

package object syntax {
  implicit final class EqOps[A](private val self: A) extends AnyVal {
    def ===(other: A)(implicit A: Eq[A]): Boolean =
      A.eqv(self, other)

    def =!=(other: A)(implicit A: Eq[A]): Boolean =
      A.neqv(self, other)
  }

  implicit final class ShowOps[A](private val self: A) extends AnyVal {
    def show(implicit A: Show[A]): String =
      A.show(self)
  }

  implicit final class SemigroupOps[A](private val self: A) extends AnyVal {
    def combine(other: A)(implicit A: Semigroup[A]): A =
      A.combine(self, other)
  }

  implicit final class FunctorOps[F[_], A](private val self: F[A]) extends AnyVal {
    def fmap[B](f: A => B)(implicit F: Functor[F]): F[B] =
      F.map(self)(f)
  }

  implicit final class MonadOps[F[_], A](private val self: F[A]) extends AnyVal {
    def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): F[B] =
      F.flatMap(self)(f)
  }

  implicit final class FoldableOps[F[_], A](private val self: F[A]) extends AnyVal {
    def foldMap[B](f: A => B)(implicit F: Foldable[F], B: Monoid[B]): B =
      F.foldMap(self)(f)
  }
}
