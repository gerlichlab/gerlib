package at.ac.oeaw.imba.gerlich.gerlib.roi

import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

/** Functionality common to ROI-related test suites. */
trait RoiTestSuite:
  given Arbitrary[PositiveInt] = Arbitrary:
    Gen
      .posNum[Int]
      .map(n =>
        PositiveInt
          .option(n)
          .getOrElse { throw IllegalRefinement(n, "Cannot refine as positive") }
      )
end RoiTestSuite
