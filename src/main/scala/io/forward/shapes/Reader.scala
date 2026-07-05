package io.forward.shapes

final case class Reader[-R, +A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] =
    Reader(env => f(run(env)))

  def flatMap[RR <: R, B](f: A => Reader[RR, B]): Reader[RR, B] =
    Reader(env => f(run(env)).run(env))
}

object Reader {
  def ask[R]: Reader[R, R] =
    Reader(identity)

  def pure[R, A](value: A): Reader[R, A] =
    Reader(_ => value)
}
