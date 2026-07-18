package io.forward.shapes

trait Eq[A] {
  def eqv(left: A, right: A): Boolean

  def neqv(left: A, right: A): Boolean =
    !eqv(left, right)
}

object Eq {
  def apply[A](implicit instance: Eq[A]): Eq[A] =
    instance

  def from[A](compare: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(left: A, right: A): Boolean =
        compare(left, right)
    }
}

trait Show[A] {
  def show(value: A): String
}

object Show {
  def apply[A](implicit instance: Show[A]): Show[A] =
    instance

  def from[A](render: A => String): Show[A] =
    new Show[A] {
      def show(value: A): String =
        render(value)
    }
}

trait Semigroup[A] {
  def combine(left: A, right: A): A
}

object Semigroup {
  def apply[A](implicit instance: Semigroup[A]): Semigroup[A] =
    instance

  def from[A](combineValues: (A, A) => A): Semigroup[A] =
    new Semigroup[A] {
      def combine(left: A, right: A): A =
        combineValues(left, right)
    }
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit instance: Monoid[A]): Monoid[A] =
    instance

  def instance[A](emptyValue: A)(combineValues: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty: A =
        emptyValue

      def combine(left: A, right: A): A =
        combineValues(left, right)
    }
}

trait Functor[F[_]] {
  def map[A, B](value: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit instance: Functor[F]): Functor[F] =
    instance
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](value: A): F[A]

  def ap[A, B](function: F[A => B])(value: F[A]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] =
    ap(pure(f))(value)

  def product[A, B](left: F[A], right: F[B]): F[(A, B)] =
    map2(left, right)((_, _))

  def map2[A, B, C](left: F[A], right: F[B])(f: (A, B) => C): F[C] =
    ap(map(left)(a => (b: B) => f(a, b)))(right)
}

object Applicative {
  def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] =
    instance
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def tailRecM[A, B](initial: A)(f: A => F[Either[A, B]]): F[B]

  def ap[A, B](function: F[A => B])(value: F[A]): F[B] =
    flatMap(function)(f => map(value)(f))

  override def map[A, B](value: F[A])(f: A => B): F[B] =
    flatMap(value)(a => pure(f(a)))
}

object Monad {
  def apply[F[_]](implicit instance: Monad[F]): Monad[F] =
    instance
}

trait Foldable[F[_]] {
  def foldLeft[A, B](value: F[A], initial: B)(f: (B, A) => B): B

  def foldMap[A, B](value: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(value, B.empty)((acc, next) => B.combine(acc, f(next)))
}

object Foldable {
  def apply[F[_]](implicit instance: Foldable[F]): Foldable[F] =
    instance
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_], A, B](value: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_], A](value: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(value)(identity)

  def map[A, B](value: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](value)(f)(using instances.idMonad)
}

object Traverse {
  def apply[F[_]](implicit instance: Traverse[F]): Traverse[F] =
    instance
}
