package at.ac.oeaw.imba.gerlich.gerlib.geometry

import scala.util.NotGiven
import scala.util.chaining.* // for pipe
import cats.*
import cats.data.Validated
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Piecewise / by-component distance, as absolute differences
  *
  * @param x
  *   The x-component of the absolute difference between two points' coordinatess
  * @param y
  *   The y-component of the absolute difference between two points' coordinates
  * @param z
  *   The z-component of the absolute difference between two points' coordinates
  */
final class PiecewiseDistance private (
    x: NonnegativeReal,
    y: NonnegativeReal,
    z: NonnegativeReal
):
  def getX: NonnegativeReal = x
  def getY: NonnegativeReal = y
  def getZ: NonnegativeReal = z

/** Helpers for working with distances in by-component / piecewise fashion */
object PiecewiseDistance:

  /** Distance threshold in which predicate comparing values to this threshold operates
    * conjunctively over components
    */
  final case class Conjunctive(get: NonnegativeReal)

  /** Compute the piecewise / component-wise distance between the given points.
    *
    * @tparam C
    *   The type of raw value wrapped in a coordinate for each 3D point
    * @param a
    *   One point
    * @param b
    *   The other point
    * @return
    *   A wrapper with access to the (absolute) difference between each component / dimension of the
    *   two given points' coordinates
    * @throws java.lang.ArithmeticException
    *   if taking any absolute difference fails to refine as nonnegative
    */
  def between[C: Numeric](a: Point3D[C], b: Point3D[C]): PiecewiseDistance =
    val xNel = absoluteComponentDifference(a.x, b.x).toValidatedNel
    val yNel = absoluteComponentDifference(a.y, b.y).toValidatedNel
    val zNel = absoluteComponentDifference(a.z, b.z).toValidatedNel
    (xNel, yNel, zNel).tupled match
    case Validated.Valid((delX, delY, delZ)) =>
      PiecewiseDistance(x = delX, y = delY, z = delZ)
    case Validated.Invalid(es) =>
      throw new ArithmeticException:
        s"Computing distance between point $a and point $b yielded ${es.length} error(s): ${es.mkString_("; ")}"

  /** Are points closer than given threshold along each axis? */
  def within[C: Numeric](
      threshold: Conjunctive
  )(a: Point3D[C], b: Point3D[C]): Boolean =
    val d = between(a, b)
    d.getX < threshold.get && d.getY < threshold.get && d.getZ < threshold.get

  private def absoluteComponentDifference[X: Numeric, C[X] <: Coordinate[X]: [C[X]] =>> NotGiven[
    C[X] =:= Coordinate[X]
  ]](a: C[X], b: C[X]): Either[String, NonnegativeReal] =
    import scala.math.Numeric.Implicits.infixNumericOps
    NonnegativeReal.either((a.value - b.value).toDouble.abs)
end PiecewiseDistance

/** Semantic wrapper to denote that a nonnegative length represents a Euclidean distance
  */
final case class EuclideanDistance(get: Distance):
  final def isFinite = get.value.isFinite
  final def isInfinite = !isFinite
end EuclideanDistance

/** Helpers for working with Euclidean distances */
object EuclideanDistance:
  /** Order distance by the wrapped value. */
  given Order[EuclideanDistance] =
    import Distance.given_Order_Distance
    Order.by(_.get) // use the Double backing the squants.space.Length.
end EuclideanDistance
