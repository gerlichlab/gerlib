package at.ac.oeaw.imba.gerlich.gerlib.geometry
package instances

import cats.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.instances.coordinate.given

/** Typeclass instances related to the notion of a point in space */
trait PointInstances:
  given monoidForPoint[A](using raw: Monoid[A]): Monoid[Point3D[A]] with
    override def empty: Point3D[A] = Point3D(
      summon[Monoid[XCoordinate[A]]].empty,
      summon[Monoid[YCoordinate[A]]].empty,
      summon[Monoid[ZCoordinate[A]]].empty
    )
    override def combine(a: Point3D[A], b: Point3D[A]): Point3D[A] = Point3D(
      a.x |+| b.x,
      a.y |+| b.y,
      a.z |+| b.z
    )
end PointInstances
