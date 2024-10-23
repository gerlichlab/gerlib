package at.ac.oeaw.imba.gerlich.gerlib

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

  type AxisX = EuclideanAxis.X.type

  type AxisY = EuclideanAxis.Y.type

  type AxisZ = EuclideanAxis.Z.type
end geometry
