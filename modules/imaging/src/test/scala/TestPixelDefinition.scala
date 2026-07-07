package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.syntax.all.*
import squants.space.{Length, LengthUnit, Microns, Nanometers}
import org.scalacheck.*
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for the parsing of pixel definitions */
class TestPixelDefinition extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:

  private type MicroOrNano = Microns.type | Nanometers.type

  private def maxSize: Double = 1e6

  private def genMicroOrNano: Gen[MicroOrNano] = Gen.oneOf(Microns, Nanometers)

  test(
    "With squants input, PixelDefinition.tryToDefine works exactly when the value is (strictly) positive."
  ):
    def genLength = genMicroOrNano.flatMap { buildLength =>
      Gen.choose(-1e3, 1e3).map(buildLength.apply)
    }
    def genPixelCount: Gen[Int] = Gen.choose(0, 1e6.toInt)

    forAll(genLength, genPixelCount, minSuccessful(10000)) { (length, numPixels) =>
      PixelDefinition.tryToDefine(length) match {
      case Left(_) if length.value <= 0 => succeed
      case Left(msg) =>
        fail(s"Pixel definition with length ${length} failed: $msg")
      case Right(_) if length.value <= 0 =>
        fail(s"Pixel definition with length ${length} succeded")
      case Right(pxDef) =>
        import PixelDefinition.syntax.lift
        given Equality[Double] =
          TolerantNumerics.tolerantDoubleEquality(1 / maxSize)

        val obs: Length = (pxDef.lift(numPixels) in length.unit)
        val exp: Length =
          val x = numPixels * length.value
          val u = length.unit.symbol
          Length(x, u).toEither
            .leftMap { e =>
              s"Error bulding length from ($x, $u: ${e.getMessage})"
            }
            .fold(msg => throw new Exception(msg), identity)
        obs.value shouldEqual exp.value
        obs.unit shouldEqual exp.unit
      }
    }
end TestPixelDefinition
