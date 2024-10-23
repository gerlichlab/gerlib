package at.ac.oeaw.imba.gerlich.gerlib.testing

import org.scalacheck.Arbitrary // Save importing this in every snippet.
import org.scalatest.funsuite.AnyFunSuite

/** Tests for availability of typeclass instances */
class TestInstanceAvailability extends AnyFunSuite:
  test("Arbitrary[FieldOfViewLike] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.imaging.FieldOfViewLike // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[FieldOfViewLike]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[FieldOfViewLike]]"
    }
    assertTypeError {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[FieldOfViewLike]]"
    }
    assertCompiles {
      "import io.github.iltotore.iron.scalacheck.all.given; import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[FieldOfViewLike]]"
    }
  }

  test("Arbitrary[ImagingTimepoint] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingTimepoint // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[ImagingTimepoint]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[ImagingTimepoint]]"
    }
    assertCompiles {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[ImagingTimepoint]]"
    }
  }

  test("Arbitrary[ImagingChannel] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingChannel // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[ImagingChannel]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[ImagingChannel]]"
    }
    assertCompiles {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[ImagingChannel]]"
    }
  }

  test("Arbitrary[ImagingContext] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingContext // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[ImagingContext]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[ImagingContext]]"
    }
    assertTypeError {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[ImagingContext]]"
    }
    assertCompiles {
      "import io.github.iltotore.iron.scalacheck.all.given; import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[ImagingContext]]"
    }
  }

  test("Arbitrary[Area] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.Area // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[Area]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[Area]]"
    }
    assertCompiles {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[Area]]"
    }
  }

  test("Arbitrary[MeanIntensity] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.MeanIntensity // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[MeanIntensity]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[MeanIntensity]]"
    }
    assertCompiles {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[MeanIntensity]]"
    }
  }

  test("Arbitrary[DetectedSpot[Double]] is correctly available.") {
    import at.ac.oeaw.imba.gerlich.gerlib.roi.DetectedSpot // Save importing this in every snippet.
    assertTypeError:
      "summon[Arbitrary[DetectedSpot[Double]]]"
    assertTypeError {
      "import io.github.iltotore.iron.scalacheck.all.given; summon[Arbitrary[DetectedSpot[Double]]]"
    }
    assertTypeError {
      "import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[DetectedSpot[Double]]]"
    }
    assertCompiles {
      "import io.github.iltotore.iron.scalacheck.all.given; import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given; summon[Arbitrary[DetectedSpot[Double]]]"
    }
  }
end TestInstanceAvailability
