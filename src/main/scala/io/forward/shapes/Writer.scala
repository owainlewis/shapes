package io.forward.shapes

final case class Writer[W, +A](log: W, value: A) {
  def map[B](f: A => B): Writer[W, B] =
    Writer(log, f(value))

  def flatMap[B](f: A => Writer[W, B])(implicit W: Semigroup[W]): Writer[W, B] = {
    val next = f(value)
    Writer(W.combine(log, next.log), next.value)
  }
}

object Writer {
  def value[W, A](value: A)(implicit W: Monoid[W]): Writer[W, A] =
    Writer(W.empty, value)

  def tell[W](log: W): Writer[W, Unit] =
    Writer(log, ())
}
