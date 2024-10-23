package at.ac.oeaw.imba.gerlich.gerlib.imaging

import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.char.*
import io.github.iltotore.iron.scalacheck.char.given
import io.github.iltotore.iron.scalacheck.numeric.intervalArbitrary

import at.ac.oeaw.imba.gerlich.gerlib.imaging.instances.fieldOfViewLike.given
import at.ac.oeaw.imba.gerlich.gerlib.numeric.{Nonnegative, NonnegativeInt}
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Tests for imaging-related types' typeclass instances */
class TestImagingInstances extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // Boost up the minimum number of successes since the tests are fast, but the
    // space of cases is complex.
    PropertyCheckConfiguration(minSuccessful = 10000)

  given Arbitrary[PositionName] =
    type GoodChar = Char :| PositionNameCharacterConstraint
    val goodPunctuation: Set[GoodChar] =
      import io.github.iltotore.iron.autoRefine
      Set('-', '.', '_')
    def genPunct: Gen[GoodChar] = Gen.oneOf(goodPunctuation)
    def genPosNameChar: Gen[GoodChar] = Gen.oneOf(
      Arbitrary.arbitrary[Char :| Letter].map(_.asInstanceOf[GoodChar]),
      Arbitrary.arbitrary[Char :| Digit].map(_.asInstanceOf[GoodChar]),
      genPunct
    )
    Arbitrary {
      Gen
        .nonEmptyListOf(genPosNameChar)
        .map(chars => chars.mkString(""))
        .suchThat { s =>
          s.toList.filter(_.isLetter) match
            case Nil =>
              !(
                // Check that the string doesn't encode an integer or decimal.
                raw"-?[0-9]+".r.matches(s) ||
                  raw"-?[0-9]+\.[0-9]+".r.matches(s)
              )
            case 'E' :: Nil =>
              !(
                // Check that the string isn't a scientific notation.
                raw"-?[0-9]\.[0-9]+E-?[0-9]{1,3}".r.matches(s) ||
                  raw"-?[0-9]E-?[0-9]{1,3}".r.matches(s)
              )
            case _ => true
        }
        .map(PositionName.unsafe)
    }

  test("PositionName is shown by its underling value.") {
    forAll { (posName: PositionName) =>
      posName.show_ shouldEqual posName.get
    }
  }

  test(
    "PositionName roundtrips: posName.show_ `andThen` PositionName.unsafe is identity."
  ) {
    forAll { (posName: PositionName) =>
      posName shouldEqual PositionName.unsafe(posName.show_)
    }
  }

  test(
    "FieldOfView is written as simple integer through its JsonValueWriter instance."
  ) {
    import upickle.default.*
    import at.ac.oeaw.imba.gerlich.gerlib.json.syntax.*

    given Arbitrary[FieldOfView] = Arbitrary {
      intervalArbitrary[Int, Nonnegative](0, Int.MaxValue).arbitrary
        .map(FieldOfView.apply)
    }

    forAll { (fov: FieldOfView) => write(fov.asJson) shouldEqual s"${fov.get}" }
  }
end TestImagingInstances
