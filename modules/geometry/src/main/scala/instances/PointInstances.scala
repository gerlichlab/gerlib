package at.ac.oeaw.imba.gerlich.gerlib.geometry
package instances

import cats.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.instances.coordinate.given

/** Typeclass instances related to the notion of a point in space */
trait PointInstances:
  /** Make points monoidally combinable when the underlying value type is such.
    */
  given [A] => (Monoid[A]) => Monoid[Point3D[A]]:
    /** Simply create the base value of each coordinate. */
    override def empty: Point3D[A] = Point3D(
      summon[Monoid[XCoordinate[A]]].empty,
      summon[Monoid[YCoordinate[A]]].empty,
      summon[Monoid[ZCoordinate[A]]].empty
    )

    /** Simply combine component-wise, then re-wrap. */
    override def combine(a: Point3D[A], b: Point3D[A]): Point3D[A] = Point3D(
      a.x |+| b.x,
      a.y |+| b.y,
      a.z |+| b.z
    )
end PointInstances
