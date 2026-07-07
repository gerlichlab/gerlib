package at.ac.oeaw.imba.gerlich.gerlib.configuration

import cats.*
import cats.syntax.all.*
import pureconfig.*
import squants.space.{LengthUnit, Microns, Nanometers}
import org.scalacheck.*
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import at.ac.oeaw.imba.gerlich.gerlib.configuration.instances.all.given // Bring in the pureconfig.ConfigReader instances.
import at.ac.oeaw.imba.gerlich.gerlib.imaging.Pixels3D

/** Tests for the parsing of pixel definitions */
class TestPixelDefinitionConfiguration
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      should.Matchers:

  private type MicroOrNano = Microns.type | Nanometers.type

  private def maxSize: Double = 1e6

  private def genMicroOrNano: Gen[MicroOrNano] = Gen.oneOf(Microns, Nanometers)

  private def genPlusMinusMillion: Gen[Double] = Gen.choose(-maxSize, maxSize)

  private type LengthParseInputs[A] = (A, LengthUnit)

  private def buildRawConfig[A: Show](
      x: LengthParseInputs[A],
      y: LengthParseInputs[A],
      z: LengthParseInputs[A]
  ): String =
    given Show[LengthParseInputs[A]] = Show.show { case (num, unit) =>
      s"${num.show} ${unit.symbol}"
    }
    s"{ x: ${x.show}, y: ${y.show}, z: ${z.show} }"

  test("Basic examples parse as expected."):
    given Arbitrary[Double] = Arbitrary(genPlusMinusMillion)
    given Shrink[Double] = Shrink.shrinkAny
    given Equality[Double] =
      TolerantNumerics.tolerantDoubleEquality(1 / maxSize)

    given Arbitrary[LengthUnit] = Arbitrary { genMicroOrNano }

    forAll(minSuccessful(10000)) { (x: Double, y: Double, z: Double, unit: LengthUnit) =>
      val perX = 100
      val perY = 200
      val perZ = 400
      val rawConfigData =
        buildRawConfig(perX -> unit, perY -> unit, perZ -> unit)
      ConfigSource.string(rawConfigData).load[Pixels3D] match {
      case Left(errors) => fail(s"Errors: $errors")
      case Right(scaling) =>
        (scaling.liftX(x) in unit).value shouldEqual x * perX
        (scaling.liftY(y) in unit).value shouldEqual y * perY
        (scaling.liftZ(z) in unit).value shouldEqual z * perZ
      }
    }

  test("Pixels3D parse requires the correct combination (x, y, z) of keys."):
    val legitKeys = Set("x", "y", "z")

    def genSubstitutions: Gen[List[(String, String)]] =
      Gen
        .containerOf[Set, String](Gen.oneOf(legitKeys))
        .map(_.toList)
        .flatMap { toSwapOut =>
          Gen.listOfN(toSwapOut.length, Gen.alphaNumStr).map(toSwapOut.zip)
        }

    val MyUnit = Nanometers
    def genInputAndExpectation: Gen[(String, Boolean)] =
      val base = buildRawConfig(100 -> MyUnit, 200 -> MyUnit, 300 -> MyUnit)
      genSubstitutions.map { subs =>
        val updated = subs.foldLeft(base) { case (acc, (oldKey, newKey)) =>
          acc.replace(oldKey ++ ":", newKey ++ ":")
        }
        val expSuccess =
          val newKeys =
            legitKeys -- subs.map(_._1).toSet ++ subs.map(_._2).toSet
          subs.isEmpty || legitKeys === newKeys
        updated -> expSuccess
      }

    given [A] => Shrink[A] = Shrink.shrinkAny // no shrinking whatsoever

    forAll(genInputAndExpectation, minSuccessful(1000)) { (rawConfigData, shouldSucceed) =>
      ConfigSource.string(rawConfigData).load[Pixels3D] match {
      case Left(errors) =>
        if !shouldSucceed then succeed
        else
          fail(
            s"Expected succeess with $rawConfigData but failed: ${errors.prettyPrint}"
          )
      case Right(_) =>
        if shouldSucceed then succeed
        else fail(s"Expected failure with $rawConfigData but succeeded")
      }
    }

  test(
    "Pixels3D parse requires proper length units with strictly positive values."
  ):
    val inputsAndExpectations = Table(
      ("rawConfigData", "shouldSucceed"),
      ("{ x: 100 nm, y: 200 nm, z: 300 nm }", true), // good example
      ("{ x: 100 nm, y: 200, z: 300 nm }", false), // missing a units on y
      ("{ x: 100 nm, y: 200 nm, z: -300 nm }", false) // negative z
    )
    forAll(inputsAndExpectations) { (rawConfigData, shouldSucceed) =>
      ConfigSource.string(rawConfigData).load[Pixels3D] match {
      case Left(errors) =>
        if !shouldSucceed then succeed
        else
          fail(
            s"Expected succeess with $rawConfigData but failed: ${errors.prettyPrint}"
          )
      case Right(_) =>
        if shouldSucceed then succeed
        else fail(s"Expected failure with $rawConfigData but succeeded")
      }

    }
end TestPixelDefinitionConfiguration
