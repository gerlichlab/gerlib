package at.ac.oeaw.imba.gerlich.gerlib.geometry

import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.all.*

/** Syntax enrichment on values of data types related to geometry */
package object syntax:

  extension [C](points: NonEmptyList[Point3D[C]])
    /** Take the centroid simply as the arithmetic mean of the points. */
    def centroid(using Monoid[Point3D[C]], Fractional[C]): Point3D[C] =
      import scala.math.Fractional.Implicits.infixFractionalOps
      val total = points.combineAll
      val n = summon[Fractional[C]].fromInt(points.length)
      Point3D(
        XCoordinate(total.x.value / n),
        YCoordinate(total.y.value / n),
        ZCoordinate(total.z.value / n)
      )

  extension [C](a: Point3D[C])
    infix def subtract(b: Point3D[C])(using num: Numeric[C]): Point3D[C] =
      import scala.math.Numeric.Implicits.infixNumericOps // Numeric.subtract --> (-)
      Point3D(
        XCoordinate(a.x.value - b.x.value),
        YCoordinate(a.y.value - b.y.value),
        ZCoordinate(a.z.value - b.z.value)
      )
