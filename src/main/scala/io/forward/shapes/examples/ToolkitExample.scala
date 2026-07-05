package io.forward.shapes.examples

import io.forward.shapes._
import io.forward.shapes.instances._
import io.forward.shapes.syntax._

object ToolkitExample {
  val total: Int =
    NonEmptyList.of(1, 2, 3).foldMap(identity)

  val parsed: Result[String, Int] =
    Result.success(41).map(_ + 1)

  val validation: Validated[NonEmptyList[String], Int] = {
    val A = Applicative[[A] =>> Validated[NonEmptyList[String], A]]
    val name: Validated[NonEmptyList[String], Int] =
      Invalid(NonEmptyList.one("missing name"))
    val age: Validated[NonEmptyList[String], Int] =
      Invalid(NonEmptyList.one("bad age"))

    A.map2(name, age)(_ + _)
  }

  val rendered: String =
    (Just(total): Maybe[Int]).show
}
