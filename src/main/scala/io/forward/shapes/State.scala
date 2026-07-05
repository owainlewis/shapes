package io.forward.shapes

final case class State[S, +A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] =
    State { state =>
      val (nextState, value) = run(state)
      (nextState, f(value))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { state =>
      val (nextState, value) = run(state)
      f(value).run(nextState)
    }
}

object State {
  def get[S]: State[S, S] =
    State(state => (state, state))

  def set[S](state: S): State[S, Unit] =
    State(_ => (state, ()))

  def modify[S](f: S => S): State[S, Unit] =
    State(state => (f(state), ()))

  def pure[S, A](value: A): State[S, A] =
    State(state => (state, value))
}
