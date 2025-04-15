package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import scala.util.NotGiven
import cats.Order
import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.{Negative, Positive}

import org.scalacheck.*
import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.geometry.instances.coordinate.given
import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingContext
import at.ac.oeaw.imba.gerlich.gerlib.roi.DetectedSpot
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.{Area, MeanIntensity}
import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.catsScalacheck.given

/** Testing-related typeclass instances for ROI-related data types */
trait RoiInstances:
  given [A] => (
      Order[A],
      Arbitrary[ZCoordinate[A]],
      Arbitrary[YCoordinate[A]],
      Arbitrary[XCoordinate[A]]
  ) => Arbitrary[BoundingBox[A]] = (
    arbitraryInterval[A, XCoordinate[A]],
    arbitraryInterval[A, YCoordinate[A]],
    arbitraryInterval[A, ZCoordinate[A]]
  ).mapN(BoundingBox.apply)

  given [C] => (
      arbCtx: Arbitrary[ImagingContext],
      arbCenter: Arbitrary[Centroid[C]],
      arbArea: Arbitrary[Area],
      arbIntensity: Arbitrary[MeanIntensity]
  ) => Arbitrary[DetectedSpot[C]] =
    (arbCtx, arbCenter, arbArea, arbIntensity).mapN(DetectedSpot.apply)

  given (arbRaw: Arbitrary[Double :| Positive]) => Arbitrary[Area] =
    arbRaw.map(Area.apply)

  given (arbRaw: Arbitrary[Double :| Not[Negative]]) => Arbitrary[MeanIntensity] =
    arbRaw.map(MeanIntensity.apply)

  /** Generate an arbitrary interval along a particular axis. */
  private def arbitraryInterval[A: Order, C <: Coordinate[A]: [C] =>> NotGiven[
    C =:= Coordinate[A]
  ]](using arbC: Arbitrary[C]): Arbitrary[BoundingBox.Interval[A, C]] =
    Arbitrary {
      Arbitrary
        .arbitrary[(C, C)]
        .map:
          case t @ (a, b) => if a > b then b -> a else t
        .map(BoundingBox.Interval.apply[A, C])
    }
end RoiInstances
