package at.ac.oeaw.imba.gerlich.gerlib.geometry

import scala.util.{NotGiven, Random}
import cats.Order
import cats.syntax.order.*
import org.scalacheck.{Arbitrary, Gen}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for the geometric coordinate abstractions */
class TestCoordinates
    extends AnyFunSuite,
      should.Matchers,
      ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  test("Ordering coordinates works.") {
    def assertOrder[A: Numeric: Order, C[A] <: Coordinate[A]: [C[
        A
    ]] =>> NotGiven[C[A] =:= Coordinate[A]]](
        rawValues: List[A],
        build: A => C[A]
    ) = {
      val orderedFirst = rawValues.sorted.map(build)
      val builtFirst = Random
        .shuffle(rawValues)
        .map(build)
        .sorted(summon[Order[C[A]]].toOrdering)
      orderedFirst shouldEqual builtFirst
    }

    enum CoordinateKey:
      case X, Y, Z

    given arbCoordinateKey: Arbitrary[CoordinateKey] =
      Arbitrary { Gen.oneOf(CoordinateKey.X, CoordinateKey.Y, CoordinateKey.Z) }

    forAll { (arbKey: CoordinateKey, arbValues: List[Double]) =>
      (arbKey, arbValues) match {
        case (CoordinateKey.X, xs) => assertOrder(xs, XCoordinate.apply)
        case (CoordinateKey.Y, ys) => assertOrder(ys, YCoordinate.apply)
        case (CoordinateKey.Z, zs) => assertOrder(zs, ZCoordinate.apply)
      }
    }
  }

  test(
    "Coordinates to order cannot be of different wrapper types, even if wrapping the same type."
  ) {
    /* Positive (no-error) pretests / controls */
    assertCompiles("XCoordinate(1.0) < XCoordinate(2.0)")
    assertCompiles("YCoordinate(-1.0) >= YCoordinate(2.0)")
    assertCompiles("ZCoordinate(1.0) <= ZCoordinate(2.0)")

    /* Negative (error) cases */
    assertTypeError("XCoordinate(1.0) < YCoordinate(2.0)")
    assertTypeError("YCoordinate(-1.0) >= ZCoordinate(2.0)")
    assertTypeError("ZCoordinate(1.0) <= XCoordinate(2.0)")
  }

  test(
    "Coordinates to order cannot have different underlying value types, even if of the same wrapper type."
  ) {
    /* Positive (no-error) pretests / controls */
    assertCompiles("XCoordinate(1.0) < XCoordinate(2.0)")
    assertCompiles("YCoordinate(-1.0) >= YCoordinate(2.0)")
    assertCompiles("ZCoordinate(1.0) <= ZCoordinate(2.0)")

    /* Negative (error) cases */
    assertTypeError("XCoordinate(1) <= XCoordinate(2.0)")
    assertTypeError("YCoordinate(0) < YCoordinate(3.0)")
    assertTypeError("ZCoordinate(1) < ZCoordinate(2.0)")
  }

  test(
    "Coordinate to order cannot have different underlying value types, even if of the same wrapper type -- SPECIAL CASE"
  ) {
    // This compiles because of the compiler's left-to-right type inference.
    pendingUntilFixed { assertTypeError("YCoordinate(-1.0) < YCoordinate(2)") }
  }

  test(
    "Generic coordinates cannot be ordered, even if the same underlying type"
  ) {
    /* Positive (no-error) pretests / controls */
    assertCompiles("XCoordinate(1.0): Coordinate[Double]") // x1
    assertCompiles("XCoordinate(2.0): Coordinate[Double]") // x2
    assertCompiles("YCoordinate(1): Coordinate[Int]") // y1
    assertCompiles("YCoordinate(2): Coordinate[Int]") // y2
    assertCompiles("ZCoordinate(2.0): Coordinate[Double]") // z1
    assertCompiles("ZCoordinate(3): Coordinate[Int]") // z2

    /* Failures for runtime-X */
    assertTypeError {
      "(XCoordinate(1.0): Coordinate[Double]) < (XCoordinate(2.0): Coordinate[Double])"
    }
    assertTypeError {
      "(XCoordinate(1.0): Coordinate[Double]) <= (XCoordinate(2.0): Coordinate[Double])"
    }
    assertTypeError {
      "(XCoordinate(1.0): Coordinate[Double]) > (XCoordinate(2.0): Coordinate[Double])"
    }
    assertTypeError {
      "(XCoordinate(1.0): Coordinate[Double]) >= (XCoordinate(2.0): Coordinate[Double])"
    }

    /* Failures for runtime-Y */
    assertTypeError {
      "(YCoordinate(1): Coordinate[Int]) < (YCoordinate(2): Coordinate[Int])"
    }
    assertTypeError {
      "(YCoordinate(1): Coordinate[Int]) <= (YCoordinate(2): Coordinate[Int])"
    }
    assertTypeError {
      "(YCoordinate(1): Coordinate[Int]) > (YCoordinate(2): Coordinate[Int])"
    }
    assertTypeError {
      "(YCoordinate(1): Coordinate[Int]) >= (YCoordinate(2): Coordinate[Int])"
    }

    /* Failures for runtime-Z */
    assertTypeError {
      "(ZCoordinate(2.0): Coordinate[Double]) < (ZCoordinate(3): Coordinate[Double])"
    }
    assertTypeError {
      "(ZCoordinate(2.0): Coordinate[Double]) <= (ZCoordinate(3): Coordinate[Double])"
    }
    assertTypeError {
      "(ZCoordinate(2.0): Coordinate[Double]) > (ZCoordinate(3): Coordinate[Double])"
    }
    assertTypeError {
      "(ZCoordinate(2.0): Coordinate[Double]) >= (ZCoordinate(3): Coordinate[Double])"
    }
  }

  test("Coordinate cannot be extended.") {
    // TODO: assert this message -- "Cannot extend sealed trait Coordinate in a different source file"
    assertTypeError("new Coordinate[Double]{ def get = 1.0 }")
    assertTypeError("new Coordinate[Int]{ def get = 1 }")
  }
end TestCoordinates
