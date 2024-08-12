package at.ac.oeaw.imba.gerlich.gerlib
package geometry

import scala.util.{NotGiven, Try}
import cats.*
import cats.syntax.all.*

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

  /** A margin for an expansion (e.g. an interval) around a point */
  final case class Margin(get: NonnegativeReal) extends AnyVal

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
  end Interval

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
