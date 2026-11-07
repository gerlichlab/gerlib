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
    val msg2Suffix = "Allegedly nonnegative length is actually negative."

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
      // The codec's own exception: since upickle 4.4.0, a traced read wraps it with the JSON path.
      val observedException = intercept[expectedException.type] {
        read[Distance](ujson.Str(data), trace = false)
      }
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

  test("An illegal distance-like value is reported with the value's own error."):
    import geometry.given // for the ReadWriter instance

    // Not just the JSON path, which is what a traced read of the value would give.
    val data = ujson.Obj("value" -> ujson.Str("70"), "metricType" -> ujson.Str("Euclidean"))
    val error = intercept[IncompleteParseException] {
      read[at.ac.oeaw.imba.gerlich.gerlib.geometry.DistanceLike](data, trace = false)
    }
    error.getMessage shouldEqual
      "Error(s): Cannot decode distance value (\"70\"): Unable to parse Length:70"

  private def unsafeBuildLength(value: Int | Double, units: LengthUnit): Length =
    Length(s"$value ${units.symbol}").fold(throw _, identity)
end TestDistanceJsonCodec
