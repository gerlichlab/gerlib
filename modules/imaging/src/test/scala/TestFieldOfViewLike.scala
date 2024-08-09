package at.ac.oeaw.imba.gerlich.gerlib
package imaging

import org.scalatest.funsuite.AnyFunSuite

/** Tests for data types representing microscope/imaging field of view */
class TestFieldOfViewLike extends AnyFunSuite:
  test(
    "For suitable input, direct PositionName creation is possible but requires certain import."
  ) {
    assertTypeError("PositionName(\"P0001.zarr\")")
    assertCompiles {
      "import io.github.iltotore.iron.autoRefine; PositionName(\"P0001.zarr\")"
    }
  }

  test(
    "With unsuitable String, even proper imports don't permit direction construction of PositionName."
  ) {
    assertTypeError {
      "import io.github.iltotore.iron.autoRefine; PositionName(\"P00@01\")"
    }
    assertTypeError {
      "import io.github.iltotore.iron.autoRefine; PositionName(\"P000(1)\")"
    }
  }
end TestFieldOfViewLike
