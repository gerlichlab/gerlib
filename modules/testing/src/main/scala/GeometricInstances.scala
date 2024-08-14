package at.ac.oeaw.imba.gerlich.gerlib
package testing

import cats.syntax.all.*
import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*

/** Testing-related typeclass instances for geometry-related data types */
trait GeometricInstances extends CatsScalacheckInstances:
  given arbitraryForXCoordinate[C](using
      c: Arbitrary[C]
  ): Arbitrary[XCoordinate[C]] = c.fmap(XCoordinate.apply)

  given arbitraryForYCoordinate[C](using
      c: Arbitrary[C]
  ): Arbitrary[YCoordinate[C]] = c.fmap(YCoordinate.apply)

  given arbitraryForZCoordinate[C](using
      c: Arbitrary[C]
  ): Arbitrary[ZCoordinate[C]] = c.fmap(ZCoordinate.apply)

  given arbitraryForPoint3D[C](using
      x: Arbitrary[XCoordinate[C]],
      y: Arbitrary[YCoordinate[C]],
      z: Arbitrary[ZCoordinate[C]]
  ): Arbitrary[Point3D[C]] = (x, y, z).mapN(Point3D.apply)

  given arbitraryForCentroid[C](using
      arbPoint: Arbitrary[Point3D[C]]
  ): Arbitrary[Centroid[C]] =
    arbPoint.map(Centroid.fromPoint)
end GeometricInstances
