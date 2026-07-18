package io.forward.shapes

import io.forward.shapes.instances._

class DataSuite extends munit.FunSuite {
  test("Maybe maps, flatMaps, and supplies defaults") {
    val value = Just(1).map(_ + 1).flatMap(n => Just(n * 2))

    assertEquals(value, Just(4))
    assertEquals(Empty.map((n: Int) => n + 1), Empty)
    assertEquals(Empty.getOrElse(42), 42)
  }

  test("Result maps successes and leftMaps failures") {
    assertEquals(Success(1).map(_ + 1), Success(2))
    assertEquals(Failure("bad").leftMap(_.toUpperCase), Failure("BAD"))
  }

  test("Validated applicative accumulates errors") {
    val applicative = Applicative[[A] =>> Validated[NonEmptyList[String], A]]
    val left = Invalid(NonEmptyList.one("missing name"))
    val right = Invalid(NonEmptyList.one("bad age"))

    val result = applicative.map2(left, right)((_: Int) + (_: Int))

    assertEquals(result, Invalid(NonEmptyList.of("missing name", "bad age")))
  }

  test("NonEmptyList keeps at least one value") {
    val values = NonEmptyList.of(1, 2, 3)

    assertEquals(values.toList, List(1, 2, 3))
    assertEquals(values.concat(NonEmptyList.of(4, 5)).toList, List(1, 2, 3, 4, 5))
    assertEquals(NonEmptyList.fromList(Nil), Empty)
    assertEquals(NonEmptyList.fromList(List(1, 2)), Just(NonEmptyList.of(1, 2)))
  }

  test("Reader reads from a shared environment") {
    val program =
      for {
        base <- Reader.ask[Int]
        result <- Reader.pure[Int, Int](base + 2)
      } yield result * 3

    assertEquals(program.run(4), 18)
  }

  test("Writer combines logs while sequencing") {
    val program =
      Writer(List("start"), 1).flatMap(n => Writer(List("next"), n + 1))

    assertEquals(program, Writer(List("start", "next"), 2))
  }

  test("State threads state through a program") {
    val increment: State[Int, Int] =
      State { value =>
        val next = value + 1
        (next, next)
      }

    val program =
      for {
        first <- increment
        second <- increment
      } yield first + second

    assertEquals(program.run(0), (2, 3))
  }

  test("Reader, Writer, and State have monad instances") {
    val readerM = Monad[[A] =>> Reader[Int, A]]
    val writerM = Monad[[A] =>> Writer[List[String], A]]
    val stateM = Monad[[A] =>> State[Int, A]]

    val reader =
      readerM.flatMap(Reader.ask[Int])(n => Reader.pure(n + 1))
    val writer =
      writerM.flatMap(writerM.pure(1))(n => Writer(List("next"), n + 1))
    val state =
      stateM.flatMap(State.get[Int])(n => State.set(n + 1).map(_ => n))

    assertEquals(reader.run(10), 11)
    assertEquals(writer, Writer(List("next"), 2))
    assertEquals(state.run(10), (11, 10))
  }
}
