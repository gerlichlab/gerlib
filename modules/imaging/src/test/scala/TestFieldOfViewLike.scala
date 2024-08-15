package at.ac.oeaw.imba.gerlich.gerlib
package imaging

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Tests for data types representing microscope/imaging field of view */
class TestFieldOfViewLike
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      should.Matchers:
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

  test(
    "FieldOfViewLike.parse preferentially parses numeric FieldOfView rather than PositionName."
  ) {
    forAll(Gen.choose(0, Int.MaxValue)) { oldRawFov =>
      val expected: FieldOfView =
        FieldOfView
          .fromInt(oldRawFov)
          .fold(
            msg => throw new Exception(s"Failed to created FOV for test: $msg"),
            identity
          )
      FieldOfViewLike.parse(oldRawFov.show_) match {
        case Left(msg) => fail(s"Expected successful parse but got error: $msg")
        case Right(observed) => observed shouldEqual expected
      }
    }
  }
end TestFieldOfViewLike
