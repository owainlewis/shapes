package io.forward.polyfunctors.examples

import io.forward.polyfunctors._

object Simple {
  val inc = (x: Int) => x + 1
  val result = Bifunctor[Tuple2].bimap((1, 2), inc, inc)
}
