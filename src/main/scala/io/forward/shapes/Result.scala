package io.forward.shapes

sealed trait Result[+E, +A] {
  def fold[B](ifFailure: E => B)(ifSuccess: A => B): B

  def map[B](f: A => B): Result[E, B] =
    fold[Result[E, B]](Failure(_))(value => Success(f(value)))

  def flatMap[EE >: E, B](f: A => Result[EE, B]): Result[EE, B] =
    fold[Result[EE, B]](Failure(_))(f)

  def leftMap[EE](f: E => EE): Result[EE, A] =
    fold[Result[EE, A]](error => Failure(f(error)))(Success(_))

  def toEither: Either[E, A] =
    fold[Either[E, A]](Left(_))(Right(_))
}

final case class Success[+A](value: A) extends Result[Nothing, A] {
  def fold[B](ifFailure: Nothing => B)(ifSuccess: A => B): B =
    ifSuccess(value)
}

final case class Failure[+E](error: E) extends Result[E, Nothing] {
  def fold[B](ifFailure: E => B)(ifSuccess: Nothing => B): B =
    ifFailure(error)
}

object Result {
  def success[A](value: A): Result[Nothing, A] =
    Success(value)

  def failure[E](error: E): Result[E, Nothing] =
    Failure(error)

  def fromEither[E, A](value: Either[E, A]): Result[E, A] =
    value.fold(Failure(_), Success(_))
}
