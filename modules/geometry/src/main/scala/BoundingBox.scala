package at.ac.oeaw.imba.gerlich.gerlib
package geometry

import scala.util.{NotGiven, Try}
import cats.*
import cats.data.EitherNel
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.instances.coordinate.given
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Bundle the 3 intervals that define a rectangular prism in 3D. */
final case class BoundingBox[A](
    sideX: BoundingBox.Interval[A, XCoordinate[A]],
    sideY: BoundingBox.Interval[A, YCoordinate[A]],
    sideZ: BoundingBox.Interval[A, ZCoordinate[A]]
)

/** Helpers for working with the notion of a 3D bounding box */
object BoundingBox:
  given orderForBoundingBox[A](using Order[A]): Order[BoundingBox[A]] =
    Order.by {
      case BoundingBox(
            BoundingBox.Interval(XCoordinate(loX), XCoordinate(hiX)),
            BoundingBox.Interval(YCoordinate(loY), YCoordinate(hiY)),
            BoundingBox.Interval(ZCoordinate(loZ), ZCoordinate(hiZ))
          ) =>
        (loX, loY, loZ, hiX, hiY, hiZ)
    }

  def around[A: Numeric](point: Point3D[A]): Dimensions => BoundingBox[Double] =
    import scala.math.Numeric.Implicits.infixNumericOps
    dims =>
      val sideX = Interval(
        XCoordinate(point.x.value.toDouble - 0.5 * dims.x.value),
        XCoordinate(point.x.value.toDouble + 0.5 * dims.x.value)
      )
      val sideY = Interval(
        YCoordinate(point.y.value.toDouble - 0.5 * dims.y.value),
        YCoordinate(point.y.value.toDouble + 0.5 * dims.y.value)
      )
      val sideZ = Interval(
        ZCoordinate(point.z.value.toDouble - 0.5 * dims.z.value),
        ZCoordinate(point.z.value.toDouble + 0.5 * dims.z.value)
      )
      BoundingBox(sideX, sideY, sideZ)

  /** An 1D interval is defined by its endpoints.
    *
    * @param lo
    *   The lower bound of the interval
    * @param hi
    *   The upper bound of the interval
    */
  final case class Interval[A: Order, C <: Coordinate[A]: [C] =>> NotGiven[
    C =:= Coordinate[A]
  ]](lo: C, hi: C):
    require(lo < hi, s"Lower bound not less than upper bound: ($lo, $hi)")
    def tryGetLength(using Numeric[A]): Either[String, NonnegativeReal] =
      import scala.math.Numeric.Implicits.infixNumericOps
      NonnegativeReal.either((hi.value - lo.value).toDouble)
  end Interval

  /** The dimensions of a bounding box */
  final case class Dimensions(
      x: XCoordinate[PositiveReal],
      y: YCoordinate[PositiveReal],
      z: ZCoordinate[PositiveReal]
  )

  /** Helpers for working with the notion of dimensions of a bounding box */
  object Dimensions:
    def tryFromBox[A: Numeric](
        box: BoundingBox[A]
    ): EitherNel[String, Dimensions] =
      val xNel = box.sideX.tryGetLength
        .flatMap(PositiveReal.either)
        .map(XCoordinate.apply)
        .toValidatedNel
      val yNel = box.sideY.tryGetLength
        .flatMap(PositiveReal.either)
        .map(YCoordinate.apply)
        .toValidatedNel
      val zNel = box.sideZ.tryGetLength
        .flatMap(PositiveReal.either)
        .map(ZCoordinate.apply)
        .toValidatedNel
      (xNel, yNel, zNel).mapN(Dimensions.apply).toEither

  /** Helpers for working with the notion of intervals */
  object Interval:
    /** Safely construct the instance, checking that the bounds are coherent. */
    def fromTuple[A: Order, C <: Coordinate[A]: [C] =>> NotGiven[
      C =:= Coordinate[A]
    ]](endpoint: (C, C)): Either[String, Interval[A, C]] =
      Try { new Interval(endpoint._1, endpoint._2) }.toEither
        .leftMap { e => s"Failed to create interval: ${e.getMessage}" }

    /** Construct the instance, throwing an exception if the bounds are
      * incoherent.
      */
    def unsafeFromTuple[A: Order, C <: Coordinate[A]: [C] =>> NotGiven[
      C =:= Coordinate[A]
    ]](endpoints: (C, C)): Interval[A, C] =
      new Interval(endpoints._1, endpoints._2)
  end Interval
end BoundingBox
