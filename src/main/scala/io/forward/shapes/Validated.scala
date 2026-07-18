package io.forward.shapes

sealed trait Validated[+E, +A] {
  def fold[B](ifInvalid: E => B)(ifValid: A => B): B

  def map[B](f: A => B): Validated[E, B] =
    fold[Validated[E, B]](Invalid(_))(value => Valid(f(value)))

  def leftMap[EE](f: E => EE): Validated[EE, A] =
    fold[Validated[EE, A]](error => Invalid(f(error)))(Valid(_))

  def toResult: Result[E, A] =
    fold[Result[E, A]](Failure(_))(Success(_))
}

final case class Valid[+A](value: A) extends Validated[Nothing, A] {
  def fold[B](ifInvalid: Nothing => B)(ifValid: A => B): B =
    ifValid(value)
}

final case class Invalid[+E](error: E) extends Validated[E, Nothing] {
  def fold[B](ifInvalid: E => B)(ifValid: Nothing => B): B =
    ifInvalid(error)
}

object Validated {
  def valid[A](value: A): Validated[Nothing, A] =
    Valid(value)

  def invalid[E](error: E): Validated[E, Nothing] =
    Invalid(error)

  def fromResult[E, A](value: Result[E, A]): Validated[E, A] =
    value.fold(Invalid(_))(Valid(_))
}
