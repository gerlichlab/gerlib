package at.ac.oeaw.imba.gerlich.gerlib.geometry

import squants.space.*

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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

end TestDistance
