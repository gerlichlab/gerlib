package at.ac.oeaw.imba.gerlich.gerlib.roi

import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Tests of correctness of typeclass instances for ROI-related data types. */
class TestRoiInstances
    extends AnyFunSuite,
      ScalaCheckDrivenPropertyChecks,
      should.Matchers,
      RoiTestSuite:

  test("PivotSize roundtrips through value from its SimpleShow instance.") {
    import PivotSize.given
    forAll { (n: PositiveInt) => n shouldEqual PivotSize(n).show_.toInt }
  }
end TestRoiInstances
