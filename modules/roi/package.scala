package at.ac.oeaw.imba.gerlich.gerlib

import scala.reflect.ClassTag
import scala.util.Try

import cats.*
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all.*
import mouse.boolean.*
import com.bc.zarr.ZarrArray

import at.ac.oeaw.imba.gerlich.gerlib.geometry.Point3D
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.extrema.*
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarr.IndexMapping
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrBlockSize
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrStandardCoordinate
import at.ac.oeaw.imba.gerlich.gerlib.zarr.ZarrArrayExtras.*

/** Types and tools for working with regions of interest (ROIs) */
package object roi:
  /** Centroid of a region of interest */
  opaque type Centroid[C] = Point3D[C]

  object Centroid:
    def fromPoint[C](pt: Point3D[C]): Centroid[C] =
      (pt: Centroid[C])

  opaque type PivotSize = PositiveInt & Singleton

  object PivotSize:
    given showForPivotSize[D <: PivotSize]: Show[D] = Show.fromToString[D]

    def apply(n: PositiveInt): PivotSize = (n: PivotSize)

    extension (dividend: Int)
      infix def mod(pivot: PivotSize): Int = dividend % pivot

  opaque type Multiarray[D <: PivotSize, E] <: Array[E] = Array[E]

  object Multiarray:
    import PivotSize.*
    def maybe[D <: PivotSize, E](pivot: PivotSize)(
        arr: Array[E]
    ): Option[Multiarray[D, E]] =
      ((arr.length `mod` pivot) === 0).option { arr: Multiarray[D, E] }

  /** Typelevel refinement of positive integer as valid ROI diameter (must be
    * even)
    */
  opaque type RoiDiameter <: PositiveInt = PositiveInt

  /** Helpers for working with ROI diameters */
  object RoiDiameter:
    /** Wrap the given value as a ROI diameter if it's legal. */
    def fromInt(z: Int): Either[String, RoiDiameter] =
      PositiveInt
        .either(z)
        .flatMap { n =>
          fromPositiveInteger(n).toRight(
            s"Can't use odd value as ROI diameter: $n"
          )
        }

    /** Try to lift the given positive integer into the ROI diameter type,
      * refining it as even.
      */
    def fromPositiveInteger(n: PositiveInt): Option[RoiDiameter] =
      (n % 2 === 0).option { n: RoiDiameter }
    given showForRoiDiameter: Show[RoiDiameter] = Show.fromToString[RoiDiameter]
  end RoiDiameter

  def readRoiData(
      za: ZarrArray,
      indexMapping: IndexMapping,
      diameter: RoiDiameter
  ): (
      OmeZarrIndex.Time,
      OmeZarrIndex.Channel,
      Centroid[NonnegativeReal]
  ) => Either[NonEmptyList[String], Array[Int]] =
    (time, channel, centroid) => {
      val shape = za.getShape()
      0.5 * diameter
      val zDepthNel = Try { shape(indexMapping.zIndex) }.toEither
        .leftMap(e => s"Failed to get Z axis raw length: ${e.getMessage}")
        .flatMap(z =>
          PositiveInt
            .either(z)
            .leftMap(msg =>
              s"Cannot refine Z axis raw length (${z.show}) as positive: $msg"
            )
        )
        .toValidatedNel
      val zyxNel = getOriginZYX(centroid, diameter)
      (zDepthNel, zyxNel).tupled.toEither.flatMap { case (zDepth, (z, y, x)) =>
        val standardCoordinate =
          OmeZarrStandardCoordinate(time, channel, z, y, x)
        val blockSize = OmeZarrBlockSize(
          OmeZarrIndex.LengthTime(PositiveInt(1)),
          OmeZarrIndex.LengthChannel(PositiveInt(1)),
          OmeZarrIndex.LengthZ(zDepth),
          OmeZarrIndex.LengthY(diameter),
          OmeZarrIndex.LengthX(diameter)
        )
        za.read(indexMapping)(standardCoordinate, blockSize)
          .leftMap(NonEmptyList.one)
      }
    }

  def readRoiData[D <: PivotSize](
      za: ZarrArray,
      indexMapping: IndexMapping,
      depth: D,
      diameter: RoiDiameter
  ): (
      OmeZarrIndex.Time,
      OmeZarrIndex.Channel,
      Centroid[NonnegativeReal]
  ) => Either[NonEmptyList[String], Multiarray[D, Int]] =
    import PivotSize.given
    (time, channel, centroid) =>
      readRoiData(za, indexMapping, diameter)(time, channel, centroid).flatMap {
        rawResult =>
          Multiarray
            .maybe(depth)(rawResult)
            .toValidNel(
              s"Cannot refine array of length ${rawResult.length} as pivoting every ${depth.show} elements"
            )
            .toEither
      }

  private def getOriginZYX(
      centroid: Centroid[NonnegativeReal],
      diameter: RoiDiameter
  ): ValidatedNel[String, (OmeZarrIndex.Z, OmeZarrIndex.Y, OmeZarrIndex.X)] =
    import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeReal.given // for Subtraction[NonnegativeReal, Double, Double]
    import at.ac.oeaw.imba.gerlich.gerlib.numeric.Subtraction.*
    val halfWidth: Double = 0.5 * diameter
    val zNel = {
      val rawCoord: Double = centroid.z.get `minus` halfWidth
      OmeZarrIndex.Z
        .fromDouble(rawCoord)
        .toValidNel(
          s"Cannot refine z-component ($rawCoord) of origin as OME-ZARR index"
        )
    }
    val yNel = {
      val rawCoord = centroid.y.get `minus` halfWidth
      OmeZarrIndex.Y
        .fromDouble(rawCoord)
        .toValidNel(
          s"Cannot refine y-component ($rawCoord) of origin as OME-ZARR index"
        )
    }
    val xNel = {
      val rawCoord = centroid.x.get `minus` halfWidth
      OmeZarrIndex.X
        .fromDouble(rawCoord)
        .toValidNel(
          s"Cannot refine x-component ($rawCoord) of origin as OME-ZARR index"
        )
    }
    (zNel, yNel, xNel).tupled

  def getMaxProjectionValues[D <: PivotSize, E: AdmitsMinimum: ClassTag: Order](
      d: D
  )(arr: Multiarray[D, E]): Array[E] = {
    given maxMonoid: MaximumSeekingMonoid[E] = MaximumSeekingMonoid.instance[E]
    getFlatProjection(d)(arr)
  }

  private def getFlatProjection[D <: PivotSize, E: ClassTag: Monoid](
      d: D
  )(arr: Multiarray[D, E]): Array[E] =
    arr.zipWithIndex
      .groupBy((_, i) => i % d)
      .view
      // Not using .combineAll here, since we're working in Array and the method's not available.
      // .mapValues(_.map(_._1).toList.combineAll)
      .mapValues(_.foldLeft(Monoid[E].empty) { case (acc, (e, _)) =>
        acc |+| e
      })
      .toArray
      .sortBy(_._1)
      .map(_._2)
