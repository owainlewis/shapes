# Shapes

Small functional programming shapes for Scala 3.

Shapes keeps the old `io.forward.polyfunctors` package and adds a compact toolkit under
`io.forward.shapes`.

## Setup

This project targets Scala 3.8.4 and uses sbt 1.12.13.

```bash
./sbt test
```

The checked-in `./sbt` script downloads the sbt launcher into `.sbt-launcher/` if sbt is not
already installed.

## What is included

Type classes:

- `Eq`
- `Show`
- `Semigroup`
- `Monoid`
- `Functor`
- `Applicative`
- `Monad`
- `Foldable`
- `Traverse`

Data types:

- `Maybe`
- `Result`
- `Validated`
- `NonEmptyList`
- `Id`
- `Reader`
- `Writer`
- `State`

Helpers:

- `io.forward.shapes.instances._` for standard instances
- `io.forward.shapes.syntax._` for small syntax extensions
- Examples in `io.forward.shapes.examples`

## Example

```scala
import io.forward.shapes._
import io.forward.shapes.instances._
import io.forward.shapes.syntax._

val total =
  NonEmptyList.of(1, 2, 3).foldMap(identity)

val parsed: Result[String, Int] =
  Result.success(41).map(_ + 1)

val validated = {
  val A = Applicative[[A] =>> Validated[NonEmptyList[String], A]]
  val name: Validated[NonEmptyList[String], Int] =
    Invalid(NonEmptyList.one("missing name"))
  val age: Validated[NonEmptyList[String], Int] =
    Invalid(NonEmptyList.one("bad age"))

  A.map2(name, age)(_ + _)
}

val rendered =
  (Just(total): Maybe[Int]).show
```

## Legacy polyfunctors

The original package still works:

```scala
import io.forward.polyfunctors._

val result =
  Bifunctor[Tuple2].bimap((1, 2), _ + 1, _ + 1)
```
