package io.forward.shapes

sealed trait Maybe[+A] {
  def fold[B](ifEmpty: => B)(ifJust: A => B): B

  def map[B](f: A => B): Maybe[B] =
    fold[Maybe[B]](Maybe.empty)(value => Just(f(value)))

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    fold[Maybe[B]](Maybe.empty)(f)

  def getOrElse[B >: A](default: => B): B =
    fold(default)(identity)

  def toOption: Option[A] =
    fold[Option[A]](None)(Some(_))

  def isEmpty: Boolean =
    fold(true)(_ => false)
}

final case class Just[+A](value: A) extends Maybe[A] {
  def fold[B](ifEmpty: => B)(ifJust: A => B): B =
    ifJust(value)
}

case object Empty extends Maybe[Nothing] {
  def fold[B](ifEmpty: => B)(ifJust: Nothing => B): B =
    ifEmpty
}

object Maybe {
  def just[A](value: A): Maybe[A] =
    Just(value)

  def empty[A]: Maybe[A] =
    Empty

  def fromOption[A](value: Option[A]): Maybe[A] =
    value.fold[Maybe[A]](Empty)(Just(_))
}
