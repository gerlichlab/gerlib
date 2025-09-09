package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import squants.QuantityParseException
import squants.space.*
import upickle.default.*
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import at.ac.oeaw.imba.gerlich.gerlib.geometry.Distance
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement
import ujson.IncompleteParseException

/** Tests for positive integer refinement type */
class TestDistanceJsonCodec extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
  sealed trait Validatable[A] extends Function1[A, Boolean]

  object Validatable:
    def strictEquals[A](a: A): Validatable[A] = new:
      override def apply(other: A): Boolean = other == a
    def errorHasSuffix(suffix: String): Validatable[Exception] = new:
      override def apply(other: Exception): Boolean = other.getMessage.endsWith(suffix)

  test("Illegal distance fails with expected IncompleteParseException."):
    import geometry.given // for the ReadWriter instance

    val err1 = QuantityParseException("Unable to parse Length", "70")
    val msg2Suffix = "Allegedly nonnegative length must actually be nonnegative."

    forAll(
      Table(
        ("data", "expectedException", "check"),
        // Distance must carry units.
        (
          "70",
          err1,
          Validatable.strictEquals(err1)
        ),
        // Distance cannot be negative.
        (
          "-70 nm",
          IncompleteParseException(
            s"(parsing -70,0 nm): $msg2Suffix"
          ),
          Validatable.errorHasSuffix(msg2Suffix)
        )
      )
    ) { (data, expectedException, check) =>
      val observedException = intercept[expectedException.type] { read[Distance](ujson.Str(data)) }
      val isGood: Boolean = check.apply(observedException)
      isGood shouldEqual true
    }

  test("Legal distance roundtrips through JSON"):
    import geometry.given // for the ReadWriter instance

    given Arbitrary[Distance] = Arbitrary(
      for
        u <- Gen.oneOf(Nanometers, Microns, Millimeters)
        v <- Gen.choose(0.0, Double.MaxValue)
        l = unsafeBuildLength(v, u)
      yield Distance
        .either(l)
        .fold(_ => throw IllegalRefinement(l, "Cannot refine length as distance"), identity)
    )

    forAll { (original: Distance) =>
      val reparsed: Distance = read(write(original))
      reparsed shouldEqual original
    }

  private def unsafeBuildLength(value: Int | Double, units: LengthUnit): Length =
    Length(s"$value ${units.symbol}").fold(throw _, identity)
end TestDistanceJsonCodec
