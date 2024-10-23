package at.ac.oeaw.imba.gerlich.gerlib.roi

import cats.syntax.all.*

import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt

/** Tests of behavior of the ROI diameter data type. */
class TestRoiDiameter
    extends AnyFunSuite,
      ScalaCheckDrivenPropertyChecks,
      should.Matchers,
      RoiTestSuite:

  test("RoiDiameter.fromInt is correct.") {
    forAll { (z: Int) =>
      val result = RoiDiameter.fromInt(z)
      val expect =
        if z <= 0 then "Should be strictly positive".asLeft
        else if z % 2 =!= 0 then s"Can't use odd value as ROI diameter: $z".asLeft
        else z.asRight
      result shouldEqual expect
    }
  }

  test("RoiDiameter.fromPositiveInteger is correct.") {
    forAll { (n: PositiveInt) =>
      RoiDiameter.fromPositiveInteger(n) match
        case None if (n % 2 =!= 0)    => succeed
        case Some(d) if (n % 2 === 0) => d shouldEqual n
        case result                   => fail(s"RoiDiameter.fromPositiveInteger($n) --> $result")
    }
  }
end TestRoiDiameter
