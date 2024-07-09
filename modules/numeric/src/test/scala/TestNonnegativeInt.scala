package at.ac.oeaw.imba.gerlich.gerlib.numeric

import scala.util.Try
import cats.syntax.all.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.{Not, StrictEqual}
import io.github.iltotore.iron.constraint.numeric.{
  Greater,
  GreaterEqual,
  Negative
}
import io.github.iltotore.iron.scalacheck.numeric.intervalArbitrary

/** Tests for positive integer refinement type */
class TestNonnegativeInt
    extends AnyFunSuite,
      should.Matchers,
      ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  test(
    "NonnegativeInt correctly restricts which expressions compile by direct assignment."
  ) {
    // NB: autoRefine is required for this (raw, non-constructor) syntax of type refinement.
    assertCompiles(
      "import io.github.iltotore.iron.autoRefine; val one: NonnegativeInt = 1"
    )
    assertTypeError("val one: NonnegativeInt = 1")
    assertCompiles(
      "import io.github.iltotore.iron.autoRefine; val zero: NonnegativeInt = 0"
    )
    assertTypeError("val moinsDeux: NonnegativeInt = -2")
  }

  test(
    "NonnegativeInt correctly restricts which expressions compile by constructor syntax."
  ) {
    // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
    assertCompiles("NonnegativeInt(1)")
    assertCompiles("NonnegativeInt(0)")
    assertTypeError("NonnegativeInt(-2)")
  }

  test("NonnegativeInt.maybe behaves correctly.") {
    forAll { (z: Int) =>
      NonnegativeInt.maybe(z) match {
        case None if z < 0     => succeed
        case Some(n) if z >= 0 => z shouldEqual n
        case bad => fail(s"NonnegativeInt.maybe($z) gave bad result: $bad")
      }
    }
  }

  test("NonnegativeInt.parse behaves correctly.") {
    type InOut = (String, Either[String, NonnegativeInt])
    def genInOutLegit: Gen[InOut] =
      intervalArbitrary[Int, Not[Negative]](0, Int.MaxValue).arbitrary
        .map { n => n.toString -> n.asRight }
    def genNegative: Gen[InOut] =
      Gen
        .choose(Int.MinValue, -1)
        .map { z => z.toString -> "!(Should be strictly negative)".asLeft }
    def genRandom: Gen[InOut] =
      Arbitrary
        .arbitrary[String]
        .filter { s => Try { s.toInt }.toOption.isEmpty }
        .map { s => s -> s"Cannot read as integer: $s".asLeft }
    def genInOut: Gen[InOut] = Gen.oneOf(
      genInOutLegit,
      genNegative,
      genRandom
    )
    forAll(genInOut) { (in, out) => NonnegativeInt.parse(in) shouldEqual out }
  }

  test(
    "NonnegativeInt.unsafe behaves in accordance with its safe counterpart."
  ) {
    forAll { (z: Int) =>
      NonnegativeInt.either(z) match {
        case Left(_) =>
          assertThrows[IllegalRefinement[Int]] { NonnegativeInt.unsafe(z) }
        case Right(n) => n shouldEqual NonnegativeInt.unsafe(z)
      }
    }
  }

  test("NonnegativeInt is a transparent type alias for Int :| Not[Negative]") {
    import io.github.iltotore.iron.autoRefine

    assertCompiles {
      "val ironRef: Int :| Not[Negative] = 0; val aliased: NonnegativeInt = ironRef"
    }
    assertCompiles {
      "val aliased: NonnegativeInt = NonnegativeInt(0); val ironRef: Int :| Not[Negative] = aliased"
    }
  }

  test("NonnegativeInt's predicate is Not[Negative], not GreaterEqual[0]") {
    import io.github.iltotore.iron.{:|, autoRefine}

    /* With GreaterEqual[0] */
    assertCompiles { "val ironRef: Int :| GreaterEqual[0] = 0" }
    assertTypeError {
      "val ironRef: Int :| GreaterEqual[0] = 0; val aliased: NonnegativeInt = ironRef"
    }

    /* With Greater[0] | StrictEqual[0] */
    assertCompiles { "val ironRef: Int :| (Greater[0] | StrictEqual[0]) = 0" }
    assertTypeError {
      "val ironRef: Int :| (Greater[0] | StrictEqual[0]) = 0; val aliased: NonnegativeInt = ironRef"
    }
  }
end TestNonnegativeInt
