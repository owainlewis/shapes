package io.forward.polyfunctors

class PolyfunctorSuite extends munit.FunSuite {
  test("existing Tuple2 bifunctor instance still works") {
    val result = Bifunctor[Tuple2].bimap((1, "a"), _ + 1, _.toUpperCase)

    assertEquals(result, (2, "A"))
  }

  test("existing Tuple3 trifunctor instance still works") {
    val result = Trifunctor[Tuple3].trimap((1, "a", true), _ + 1, _.toUpperCase, !_)

    assertEquals(result, (2, "A", false))
  }
}
