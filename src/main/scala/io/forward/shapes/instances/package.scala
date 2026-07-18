package io.forward.shapes

package object instances {
  implicit val intEq: Eq[Int] =
    Eq.from(_ == _)

  implicit val stringEq: Eq[String] =
    Eq.from(_ == _)

  implicit val booleanEq: Eq[Boolean] =
    Eq.from(_ == _)

  implicit def listEq[A](implicit A: Eq[A]): Eq[List[A]] =
    Eq.from { (left, right) =>
      left.length == right.length && left.zip(right).forall { case (a, b) => A.eqv(a, b) }
    }

  implicit val intShow: Show[Int] =
    Show.from(_.toString)

  implicit val stringShow: Show[String] =
    Show.from(identity)

  implicit val booleanShow: Show[Boolean] =
    Show.from(_.toString)

  implicit val intAdditionMonoid: Monoid[Int] =
    Monoid.instance(0)(_ + _)

  implicit val stringMonoid: Monoid[String] =
    Monoid.instance("")(_ + _)

  implicit def listMonoid[A]: Monoid[List[A]] =
    Monoid.instance(List.empty[A])(_ ++ _)

  implicit val idMonad: Monad[Id] =
    new Monad[Id] {
      def pure[A](value: A): A =
        value

      def flatMap[A, B](value: A)(f: A => B): B =
        f(value)

      def tailRecM[A, B](initial: A)(f: A => Either[A, B]): B = {
        @annotation.tailrec
        def loop(current: A): B =
          f(current) match {
            case Right(value) => value
            case Left(next)   => loop(next)
          }

        loop(initial)
      }
    }

  implicit val maybeMonad: Monad[Maybe] & Traverse[Maybe] =
    new Monad[Maybe] with Traverse[Maybe] {
      def pure[A](value: A): Maybe[A] =
        Just(value)

      override def map[A, B](value: Maybe[A])(f: A => B): Maybe[B] =
        value.map(f)

      def flatMap[A, B](value: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        value.flatMap(f)

      def foldLeft[A, B](value: Maybe[A], initial: B)(f: (B, A) => B): B =
        value.fold(initial)(a => f(initial, a))

      def traverse[G[_], A, B](value: Maybe[A])(f: A => G[B])(implicit
          G: Applicative[G]
      ): G[Maybe[B]] =
        value.fold(G.pure[Maybe[B]](Empty))(a => G.map(f(a))(Just(_)))

      def tailRecM[A, B](initial: A)(f: A => Maybe[Either[A, B]]): Maybe[B] = {
        @annotation.tailrec
        def loop(current: A): Maybe[B] =
          f(current) match {
            case Empty              => Empty
            case Just(Left(next))   => loop(next)
            case Just(Right(value)) => Just(value)
          }

        loop(initial)
      }
    }

  implicit def resultMonad[E]: Monad[[A] =>> Result[E, A]] =
    new Monad[[A] =>> Result[E, A]] {
      def pure[A](value: A): Result[E, A] =
        Success(value)

      def flatMap[A, B](value: Result[E, A])(f: A => Result[E, B]): Result[E, B] =
        value.flatMap(f)

      def tailRecM[A, B](initial: A)(f: A => Result[E, Either[A, B]]): Result[E, B] = {
        @annotation.tailrec
        def loop(current: A): Result[E, B] =
          f(current) match {
            case failure @ Failure(_)  => failure
            case Success(Left(next))   => loop(next)
            case Success(Right(value)) => Success(value)
          }

        loop(initial)
      }
    }

  implicit def readerMonad[R]: Monad[[A] =>> Reader[R, A]] =
    new Monad[[A] =>> Reader[R, A]] {
      def pure[A](value: A): Reader[R, A] =
        Reader.pure(value)

      def flatMap[A, B](value: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        value.flatMap(f)

      def tailRecM[A, B](initial: A)(f: A => Reader[R, Either[A, B]]): Reader[R, B] =
        Reader { env =>
          @annotation.tailrec
          def loop(current: A): B =
            f(current).run(env) match {
              case Left(next)   => loop(next)
              case Right(value) => value
            }

          loop(initial)
        }
    }

  implicit def writerMonad[W](implicit W: Monoid[W]): Monad[[A] =>> Writer[W, A]] =
    new Monad[[A] =>> Writer[W, A]] {
      def pure[A](value: A): Writer[W, A] =
        Writer.value(value)

      def flatMap[A, B](value: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] =
        value.flatMap(f)

      def tailRecM[A, B](initial: A)(f: A => Writer[W, Either[A, B]]): Writer[W, B] = {
        @annotation.tailrec
        def loop(current: A, log: W): Writer[W, B] = {
          val next = f(current)
          val combined = W.combine(log, next.log)
          next.value match {
            case Left(value)  => loop(value, combined)
            case Right(value) => Writer(combined, value)
          }
        }

        loop(initial, W.empty)
      }
    }

  implicit def stateMonad[S]: Monad[[A] =>> State[S, A]] =
    new Monad[[A] =>> State[S, A]] {
      def pure[A](value: A): State[S, A] =
        State.pure(value)

      def flatMap[A, B](value: State[S, A])(f: A => State[S, B]): State[S, B] =
        value.flatMap(f)

      def tailRecM[A, B](initial: A)(f: A => State[S, Either[A, B]]): State[S, B] =
        State { state =>
          @annotation.tailrec
          def loop(current: A, currentState: S): (S, B) =
            f(current).run(currentState) match {
              case (nextState, Left(next)) =>
                loop(next, nextState)
              case (nextState, Right(value)) =>
                (nextState, value)
            }

          loop(initial, state)
        }
    }

  implicit val listMonad: Monad[List] & Traverse[List] =
    new Monad[List] with Traverse[List] {
      def pure[A](value: A): List[A] =
        List(value)

      override def map[A, B](value: List[A])(f: A => B): List[B] =
        value.map(f)

      def flatMap[A, B](value: List[A])(f: A => List[B]): List[B] =
        value.flatMap(f)

      def foldLeft[A, B](value: List[A], initial: B)(f: (B, A) => B): B =
        value.foldLeft(initial)(f)

      def traverse[G[_], A, B](
          value: List[A]
      )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        value.foldRight(G.pure(List.empty[B])) { (a, acc) =>
          G.map2(f(a), acc)(_ :: _)
        }

      def tailRecM[A, B](initial: A)(f: A => List[Either[A, B]]): List[B] = {
        @annotation.tailrec
        def loop(open: List[Either[A, B]], done: List[B]): List[B] =
          open match {
            case Nil =>
              done.reverse
            case Right(value) :: rest =>
              loop(rest, value :: done)
            case Left(next) :: rest =>
              loop(f(next) ++ rest, done)
          }

        loop(f(initial), Nil)
      }
    }

  implicit def maybeEq[A](implicit A: Eq[A]): Eq[Maybe[A]] =
    Eq.from {
      case (Empty, Empty)            => true
      case (Just(left), Just(right)) => A.eqv(left, right)
      case _                         => false
    }

  implicit def maybeShow[A](implicit A: Show[A]): Show[Maybe[A]] =
    Show.from(_.fold("Empty")(value => s"Just(${A.show(value)})"))

  implicit def maybeMonoid[A](implicit A: Semigroup[A]): Monoid[Maybe[A]] =
    Monoid.instance[Maybe[A]](Empty) {
      case (Empty, right) =>
        right
      case (left, Empty) =>
        left
      case (Just(left), Just(right)) =>
        Just(A.combine(left, right))
    }

  implicit def resultEq[E, A](implicit E: Eq[E], A: Eq[A]): Eq[Result[E, A]] =
    Eq.from {
      case (Failure(left), Failure(right)) => E.eqv(left, right)
      case (Success(left), Success(right)) => A.eqv(left, right)
      case _                               => false
    }

  implicit def resultShow[E, A](implicit E: Show[E], A: Show[A]): Show[Result[E, A]] =
    Show.from(_.fold(error => s"Failure(${E.show(error)})")(value => s"Success(${A.show(value)})"))

  implicit def validatedEq[E, A](implicit E: Eq[E], A: Eq[A]): Eq[Validated[E, A]] =
    Eq.from {
      case (Invalid(left), Invalid(right)) => E.eqv(left, right)
      case (Valid(left), Valid(right))     => A.eqv(left, right)
      case _                               => false
    }

  implicit def validatedShow[E, A](implicit E: Show[E], A: Show[A]): Show[Validated[E, A]] =
    Show.from(_.fold(error => s"Invalid(${E.show(error)})")(value => s"Valid(${A.show(value)})"))

  implicit def validatedApplicative[E](implicit
      E: Semigroup[E]
  ): Applicative[[A] =>> Validated[E, A]] =
    new Applicative[[A] =>> Validated[E, A]] {
      def pure[A](value: A): Validated[E, A] =
        Valid(value)

      def ap[A, B](function: Validated[E, A => B])(value: Validated[E, A]): Validated[E, B] =
        (function, value) match {
          case (Valid(f), Valid(a)) =>
            Valid(f(a))
          case (Invalid(left), Invalid(right)) =>
            Invalid(E.combine(left, right))
          case (Invalid(error), _) =>
            Invalid(error)
          case (_, Invalid(error)) =>
            Invalid(error)
        }
    }

  implicit def nonEmptyListEq[A](implicit A: Eq[A]): Eq[NonEmptyList[A]] =
    Eq.from((left, right) => listEq(using A).eqv(left.toList, right.toList))

  implicit def nonEmptyListShow[A](implicit A: Show[A]): Show[NonEmptyList[A]] =
    Show.from(value => value.toList.map(A.show).mkString("NonEmptyList(", ", ", ")"))

  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] =
    Semigroup.from(_.concat(_))

  implicit val nonEmptyListTraverse: Traverse[NonEmptyList] =
    new Traverse[NonEmptyList] {
      def foldLeft[A, B](value: NonEmptyList[A], initial: B)(f: (B, A) => B): B =
        value.foldLeft(initial)(f)

      def traverse[G[_], A, B](value: NonEmptyList[A])(f: A => G[B])(implicit
          G: Applicative[G]
      ): G[NonEmptyList[B]] =
        G.map2(f(value.head), listMonad.traverse(value.tail)(f))(NonEmptyList(_, _))
    }
}
