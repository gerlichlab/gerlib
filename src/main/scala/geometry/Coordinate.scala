package at.ac.oeaw.imba.gerlich.gerlib.geometry

// for .toDouble on a provably-Numeric
import scala.math.Numeric.Implicits.infixNumericOps

import cats.*
import cats.syntax.all.*

/** Type wrapper around a numeric value representing a spatial coordinate */
sealed trait Coordinate[A]:
    /** Access the underling, wrapped value. Requires a [[scala.math.Numeric]] instance to reflect intended use. */
    def get(using Numeric[A]): A
    /** With proof that the underlying, wrapped value is numeric, convert it to a [[scala.Double]]  */
    def toDouble(using Numeric[A]): Double = get.toDouble

final case class XCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

final case class YCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

final case class ZCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

object Coordinate:
    given showForCoordinate[A : Show]: Show[Coordinate[A]] = Show.show{
        (_: Coordinate[A]) match {
            case XCoordinate(value) => value.show
            case YCoordinate(value) => value.show
            case ZCoordinate(value) => value.show
        }
    }