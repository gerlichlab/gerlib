package at.ac.oeaw.imba.gerlich.gerlib

import cats.Order
import io.github.iltotore.iron.{RefinedType, RuntimeConstraint}
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative
import squants.space.Length

/** Types and tools related to geometry */
package object geometry:
  /** Centroid of a region of interest */
  opaque type Centroid[C] = Point3D[C]

  /** Helpers for working with {@code Centroid} values. */
  object Centroid:
    extension [C](c: Centroid[C])
      /** Allow the centroid to be used as an ordinary point, but force awareness by the caller.
        */
      def asPoint: Point3D[C] = c

    /** Semantically designate the given value as a centroid. */
    def fromPoint[C](pt: Point3D[C]): Centroid[C] =
      (pt: Centroid[C])

    extension [C](c: Centroid[C])
      /* Provide access to the centroid components. */
      /** Access x-component of centroid. */
      private[gerlib] def x: XCoordinate[C] = c.x

      /** Access y-component of centroid. */
      private[gerlib] def y: YCoordinate[C] = c.y

      /** Access z-component of centroid. */
      private[gerlib] def z: ZCoordinate[C] = c.z
  end Centroid

  /** Constrain a length to be like a distance (nonnegative). */
  given RuntimeConstraint[Length, Not[Negative]] =
    new RuntimeConstraint(
      _.value >= 0,
      "Allegedly nonnegative length must actually be nonnegative."
    )

  /** Leave this alias transparent, since we just want the typelevel 'check' that the length is
    * nonnegative; we don't want the underlying type masked.
    */
  type Distance = Distance.T

  object Distance extends RefinedType[Length, Not[Negative]]:
    given Order[Distance] = Order.fromOrdering

  type AxisX = EuclideanAxis.X.type

  type AxisY = EuclideanAxis.Y.type

  type AxisZ = EuclideanAxis.Z.type
end geometry
