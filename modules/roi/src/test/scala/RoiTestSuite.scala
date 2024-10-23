package at.ac.oeaw.imba.gerlich.gerlib.roi

import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt

/** Functionality common to ROI-related test suites. */
trait RoiTestSuite:
  given Arbitrary[PositiveInt] = Arbitrary:
    Gen.posNum[Int].map(PositiveInt.unsafe)
end RoiTestSuite
