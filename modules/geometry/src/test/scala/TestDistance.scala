package at.ac.oeaw.imba.gerlich.gerlib.geometry

import cats.syntax.all.*

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.github.iltotore.iron.{:|, autoRefine, refineUnsafe}
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative
import squants.space.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

/** Tests for the refinement of a [[squants.space.Length]] value as a distance */
class TestDistance extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:

  given Arbitrary[LengthUnit] = Arbitrary:
    Gen.oneOf(
      Angstroms,
      Nanometers,
      Microns,
      Millimeters
    )

  given (Arbitrary[Double]) => Arbitrary[Length] = Arbitrary:
    for
      v <- arbitrary[Double]
      u <- arbitrary[LengthUnit]
    yield Length(v -> u.symbol).fold(throw _, identity)

  test("Distance.option works if and only if the length is nonnegative."):
    forAll { (l: Length) =>
      (l.value < 0, Distance.option(l)) match {
      case (false, Some(d)) => d shouldEqual l
      case (true, None)     => succeed
      case (_, _)           =>
      }
    }

  test("Distance instantiation CANNOT be done with apply syntax."):
    assertCompiles("val l: Length = Length(1 -> \"nm\").get")
    assertTypeError(
      "Distance(Length(1 -> \"nm\").get)"
    ) // should be missing Constraint[Length, Not[Negative]]

  test("A value typed as Distance is correctly compared to a squants.space.Length value."):
    given Arbitrary[Distance] =
      given Arbitrary[Double] = Arbitrary(Gen.choose(0, Double.MaxValue))
      Arbitrary(
        arbitrary[Length].map(l =>
          Distance
            .option(l)
            .getOrElse(throw IllegalRefinement(l, "Cannot refine length as distance"))
        )
      )

    forAll { (l: Length, d: Distance) =>
      val dAsL: Length = d
      l < d shouldBe l < dAsL
    }

  test("EuclideanDistance.unsafe requires a length, not just int or nonnegative int."):
    assertTypeError("EuclideanDistance.unsafe(2)") // plain Int prohibited
    assertCompiles("2: Int :| Not[Negative]") // building nonnegative int works
    assertTypeError(
      "EuclideanDistance.unsafe(2: Int :| Not[Negative])"
    ) // nonnegative Int prohibited
    assertCompiles("Nanometers(2)") // building Length works
    assertCompiles("EuclideanDistance.unsafe(Nanometers(2))") // Length argument works

  test("EuclideanDistance.unsafe is correct for a nonnegative length."):
    forAll(Gen.choose(0, Int.MaxValue).map(Nanometers.apply)) { l =>
      EuclideanDistance.unsafe(l).get shouldEqual Distance.applyUnsafe(l)
    }

  test("EuclideanDistance.unsafe fails properly for a negative length."):
    forAll(Gen.choose(Int.MinValue, -1).map(Nanometers.apply)) { l =>
      val obsErr = intercept[IllegalArgumentException] { EuclideanDistance.unsafe(l) }
      obsErr.getMessage shouldEqual "Allegedly nonnegative length is actually negative."
    }

  test("Distance roundtrips through .show"):
    import ArbitraryDistance.given
    given Arbitrary[Double] = Arbitrary(Gen.choose(0, Double.MaxValue))
    forAll { (d: Distance) => Distance.parse(d.show) shouldEqual d.asRight }

  test("Distance roundtrips through .simpleShow"):
    import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.* // for .show_
    import ArbitraryDistance.given
    given Arbitrary[Double] = Arbitrary(Gen.choose(0, Double.MaxValue))
    forAll { (d: Distance) => Distance.parse(d.show_) shouldEqual d.asRight }

  object ArbitraryDistance:
    given Arbitrary[Length] => Arbitrary[Distance] =
      Arbitrary(
        arbitrary[Length]
          .map(l => Distance.either(l).fold(msg => throw IllegalRefinement(l, msg), identity))
      )
  end ArbitraryDistance
end TestDistance
