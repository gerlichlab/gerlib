package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.syntax.all.*
import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*

/** Testing-related typeclass instances for geometry-related data types */
trait GeometricInstances extends CatsScalacheckInstances:
  given [C] => (c: Arbitrary[C]) => Arbitrary[XCoordinate[C]] = c.fmap(XCoordinate.apply)

  given [C] => (c: Arbitrary[C]) => Arbitrary[YCoordinate[C]] = c.fmap(YCoordinate.apply)

  given [C] => (c: Arbitrary[C]) => Arbitrary[ZCoordinate[C]] = c.fmap(ZCoordinate.apply)

  given [C] => (
      x: Arbitrary[XCoordinate[C]],
      y: Arbitrary[YCoordinate[C]],
      z: Arbitrary[ZCoordinate[C]]
  ) => Arbitrary[Point3D[C]] = (x, y, z).mapN(Point3D.apply)

  given [C] => (arbPoint: Arbitrary[Point3D[C]]) => Arbitrary[Centroid[C]] =
    arbPoint.map(Centroid.fromPoint)
end GeometricInstances
