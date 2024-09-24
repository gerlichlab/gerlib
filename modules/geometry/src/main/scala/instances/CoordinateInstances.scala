package at.ac.oeaw.imba.gerlich.gerlib.geometry
package instances

import scala.util.NotGiven

import cats.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for coordinate data types */
trait CoordinateInstances:
  private def monoidInstance[A: Monoid, C <: Coordinate[A]: [C] =>> NotGiven[
    C =:= Coordinate[A]
  ]](lift: A => C): Monoid[C] = Monoid.instance(
    lift(summon[Monoid[A]].empty),
    (c1, c2) => lift(c1.value |+| c2.value)
  )

  given monoidForX[A: Monoid]: Monoid[XCoordinate[A]] = monoidInstance(
    XCoordinate.apply
  )

  given monoidForY[A: Monoid]: Monoid[YCoordinate[A]] = monoidInstance(
    YCoordinate.apply
  )

  given monoidForZ[A: Monoid]: Monoid[ZCoordinate[A]] = monoidInstance(
    ZCoordinate.apply
  )

  given simpleShowForCoordinate[A: SimpleShow, C <: Coordinate[
    A
  ]: [C] =>> NotGiven[C =:= Coordinate[A]]]: SimpleShow[C] =
    SimpleShow.instance { c => c.value.show_ }

  /** Use the [[cats.Order]] instance for the underlying type to order a
    * particular coordinate subtype.
    */
  given orderForCoordinate[A: Order, C <: Coordinate[A]: [C] =>> NotGiven[
    C =:= Coordinate[A]
  ]]: Order[C] =
    Order.by {
      case XCoordinate(value) => value
      case YCoordinate(value) => value
      case ZCoordinate(value) => value
    }
end CoordinateInstances
