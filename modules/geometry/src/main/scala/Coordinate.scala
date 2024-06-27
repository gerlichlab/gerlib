package at.ac.oeaw.imba.gerlich.gerlib.geometry

// to provide .toDouble syntax on a provably-Numeric
import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.NotGiven

import cats.*
import cats.syntax.all.*

/** Type wrapper around a numeric value representing a spatial coordinate */
sealed trait Coordinate[A]:
    /** Access the underling, wrapped value. Requires a [[scala.math.Numeric]] instance to reflect intended use. */
    def get(using Numeric[A]): A
    /** With proof that the underlying, wrapped value is numeric, convert it to a [[scala.Double]]  */
    def toDouble(using Numeric[A]): Double = get.toDouble

/** Wrap the given value as reprsenting a x-coordinate in space. */
final case class XCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

/** Wrap the given value as reprsenting a y-coordinate in space. */
final case class YCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

/** Wrap the given value as reprsenting a z-coordinate in space. */
final case class ZCoordinate[A](private val value: A) extends Coordinate[A]:
    def get(using Numeric[A]): A = value

/** Helpers for working with coordinates in space */
object Coordinate:
    /** Use the [[cats.Order]] instance for the underlying type to order a particular coordinate subtype. */
    given orderForCoordinate[A : Order, C[A] <: Coordinate[A] : [C[A]] =>> NotGiven[C[A] =:= Coordinate[A]]]: Order[C[A]] = 
        Order.by{
            case XCoordinate(value) => value
            case YCoordinate(value) => value
            case ZCoordinate(value) => value
        }

    /** Use the [[cats.Show]] instance for the underlying type to `.show` a coordinate which wraps it. */
    given showForCoordinate[A : Show]: Show[Coordinate[A]] = Show.show{
        (_: Coordinate[A]) match {
            case XCoordinate(value) => value.show
            case YCoordinate(value) => value.show
            case ZCoordinate(value) => value.show
        }
    }