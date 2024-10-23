package at.ac.oeaw.imba.gerlich.gerlib.numeric

import scala.annotation.nowarn
import scala.util.Try
import cats.syntax.all.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.github.iltotore.iron.constraint.numeric.Positive
import io.github.iltotore.iron.scalacheck.numeric.intervalArbitrary

/** Tests for positive integer refinement type */
class TestPositiveInt extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  test(
    "PositiveInt correctly restricts which expressions compile by direct assignment."
  ) {
    // NB: autoRefine is required for this (raw, non-constructor) syntax of type refinement.
    assertCompiles(
      "import io.github.iltotore.iron.autoRefine; val one: PositiveInt = 1"
    )
    assertTypeError("val one: PositiveInt = 1")
    assertTypeError("val zero: PositiveInt = 0")
    assertTypeError(
      "import io.github.iltotore.iron.autoRefine; val zero: PositiveInt = 0"
    )
    assertTypeError("val moinsDeux: PositiveInt = -2")
  }

  test(
    "PositiveInt correctly restricts which expressions compile by constructor syntax."
  ) {
    // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
    assertCompiles("PositiveInt(1)")
    assertTypeError("PositiveInt(0)")
    assertTypeError("PositiveInt(-2)")
  }

  test("PositiveInt.maybe behaves correctly.") {
    forAll { (z: Int) =>
      PositiveInt.maybe(z) match
        case None if z <= 0   => succeed
        case Some(n) if z > 0 => z shouldEqual n
        case bad              => fail(s"PositiveInt.maybe($z) gave bad result: $bad")
    }
  }

  test("NonnegativeInt.parse behaves correctly.") {
    type InOut = (String, Either[String, PositiveInt])
    def genInOutLegit: Gen[InOut] =
      intervalArbitrary[Int, Positive](1, Int.MaxValue).arbitrary
        .map: n =>
          n.toString -> n.asRight
    def genNegative: Gen[InOut] =
      Gen
        .choose(Int.MinValue, -1)
        .map { z => z.toString -> "!(Should be strictly negative)".asLeft }
    def genRandom: Gen[InOut] =
      Arbitrary
        .arbitrary[String]
        .filter: s =>
          Try:
            s.toInt
          .toOption.isEmpty
        .map: s =>
          s -> s"Cannot read as integer: $s".asLeft
    def genInOut: Gen[InOut] = Gen.oneOf(
      genInOutLegit,
      genNegative,
      genRandom
    )
    forAll(genInOut): (in, out) =>
      NonnegativeInt.parse(in) shouldEqual out
  }

  test("PositiveInt.unsafe behaves in accordance with its safe counterpart.") {
    forAll { (z: Int) =>
      PositiveInt.either(z) match
        case Left(_) =>
          assertThrows[IllegalRefinement[Int]]:
            PositiveInt.unsafe(z)
        case Right(n) => n shouldEqual PositiveInt.unsafe(z)
    }
  }

  test("PositiveInt is a transparent type alias for Int :| Positive") {
    import io.github.iltotore.iron.{:|, autoRefine}
    import io.github.iltotore.iron.constraint.numeric.Positive
    assertCompiles {
      "val ironRef: Int :| Positive = 1; val aliased: PositiveInt = ironRef"
    }
    assertCompiles {
      "val aliased: PositiveInt = PositiveInt(1); val ironRef: Int :| Positive = aliased"
    }
  }: @nowarn
end TestPositiveInt
