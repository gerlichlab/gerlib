package at.ac.oeaw.imba.gerlich.gerlib.imaging

import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.char.*
import io.github.iltotore.iron.scalacheck.char.given

import at.ac.oeaw.imba.gerlich.gerlib.imaging.instances.fieldOfViewLike.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Tests for imaging-related types' typeclass instances */
class TestImagingInstances
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      should.Matchers:

  def doubleQuote = (s: String) => "\"" ++ s ++ "\""

  given Arbitrary[PositionName] =
    import io.github.iltotore.iron.autoRefine
    type GoodChar = Char :| PositionNameCharacterConstraint
    def genPunct: Gen[GoodChar] = Gen.oneOf('-', '.', '_')
    def genPosNameChar: Gen[GoodChar] = Gen.oneOf(
      Arbitrary.arbitrary[Char :| Letter].map(_.asInstanceOf[GoodChar]),
      Arbitrary.arbitrary[Char :| Digit].map(_.asInstanceOf[GoodChar]),
      genPunct
    )
    Arbitrary {
      Gen
        .nonEmptyListOf(genPosNameChar)
        .map(chars => doubleQuote(chars.mkString("")))
        .map(PositionName.unsafe)
    }

  test("PositionName is shown with its underling value double-quoted.") {
    forAll { (posName: PositionName) =>
      posName.show_ shouldEqual doubleQuote(posName.get)
    }
  }

  test(
    "PositionName roundtrips: posName.show_ `andThen` PositionName.unsafe is identity."
  ) {
    forAll { (posName: PositionName) =>
      posName shouldEqual PositionName.unsafe(posName.show_)
    }
  }
end TestImagingInstances
