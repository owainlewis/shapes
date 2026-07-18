package io.forward.shapes

final case class NonEmptyList[+A](head: A, tail: List[A]) {
  def toList: List[A] =
    head :: tail

  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def prepend[B >: A](value: B): NonEmptyList[B] =
    NonEmptyList(value, toList)

  def append[B >: A](value: B): NonEmptyList[B] =
    NonEmptyList(head, tail :+ value)

  def concat[B >: A](other: NonEmptyList[B]): NonEmptyList[B] =
    NonEmptyList(head, tail ++ other.toList)

  def foldLeft[B](initial: B)(f: (B, A) => B): B =
    toList.foldLeft(initial)(f)

  def reduceLeft[B >: A](f: (B, A) => B): B =
    tail.foldLeft[B](head)(f)
}

object NonEmptyList {
  def one[A](value: A): NonEmptyList[A] =
    NonEmptyList(value, Nil)

  def of[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)

  def fromList[A](values: List[A]): Maybe[NonEmptyList[A]] =
    values match {
      case head :: tail => Just(NonEmptyList(head, tail))
      case Nil          => Empty
    }
}
