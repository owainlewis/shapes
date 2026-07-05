package io.forward.shapes

import io.forward.shapes.instances._
import io.forward.shapes.syntax._

class TypeclassSuite extends munit.FunSuite {
  test("Eq and Show syntax delegate to instances") {
    assert(1 === 1)
    assert(1 =!= 2)
    assertEquals((Just(1): Maybe[Int]).show, "Just(1)")
  }

  test("Monoid supplies an identity and associative combine operation") {
    val M = Monoid[List[Int]]

    assertEquals(M.combine(M.empty, List(1, 2)), List(1, 2))
    assertEquals(List(1).combine(List(2, 3)), List(1, 2, 3))
  }

  test("Maybe functor obeys identity and composition") {
    val F = Functor[Maybe]
    val value: Maybe[Int] = Just(10)
    val f = (n: Int) => n + 1
    val g = (n: Int) => n.toString

    assertEquals(F.map(value)(identity), value)
    assertEquals(F.map(F.map(value)(f))(g), F.map(value)(f.andThen(g)))
  }

  test("Maybe monad obeys left and right identity") {
    val M = Monad[Maybe]
    val f = (n: Int) => Just(n + 1)
    val value = Just(41)

    assertEquals(M.flatMap(M.pure(41))(f), f(41))
    assertEquals(M.flatMap(value)(M.pure), value)
  }

  test("Result monad stops at the first failure") {
    val M = Monad[[A] =>> Result[String, A]]

    val result =
      M.flatMap(Failure("nope"): Result[String, Int])(_ => Success(1))

    assertEquals(result, Failure("nope"))
  }

  test("Foldable and Traverse work over lists and Maybe") {
    val sum = List(1, 2, 3).foldMap(identity)
    val sequenced = Traverse[List].sequence(List[Maybe[Int]](Just(1), Just(2), Just(3)))
    val failed = Traverse[List].sequence(List[Maybe[Int]](Just(1), Empty, Just(3)))

    assertEquals(sum, 6)
    assertEquals(sequenced, Just(List(1, 2, 3)))
    assertEquals(failed, Empty)
  }
}
