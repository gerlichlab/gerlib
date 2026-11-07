package at.ac.oeaw.imba.gerlich.gerlib

import scala.reflect.ClassTag

import cats.*
import cats.syntax.all.*
import mouse.boolean.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.extrema.*

/** Types and tools for working with regions of interest (ROIs) */
package object roi:
  /** A specific pivot size (1D array -> 2D array) is a specific positive integer.
    */
  opaque type PivotSize = PositiveInt & Singleton

  /** Helpers for working with (array) pivot size abstraction */
  object PivotSize:
    /** Simply show the pivot size by its {@code toString} representation. */
    given [D <: PivotSize] => SimpleShow[D] = SimpleShow.fromToString[D]

    /** Designate, semantically, the given value as an array pivot size. */
    def apply(n: PositiveInt): PivotSize = (n: PivotSize)

    extension (dividend: Int)
      /** Get the remainder of the syntax-enriched value divided by the given pivot.
        */
      infix def mod(pivot: PivotSize): Int = dividend % pivot

      /** Test whether the syntax-enriched value is evenly divisible by the given pivot.
        */
      infix def isDivisibleBy(pivot: PivotSize): Boolean = mod(pivot) === 0

  /** A multi(dimensional)array is defined by an element type and a pivot size (think: number of
    * "columns", or number of values per "row").
    */
  opaque type Multiarray[D <: PivotSize, E] <: Array[E] = Array[E]

  /** Helpers for working with {@code Multiarray} values */
  object Multiarray:
    import PivotSize.*
    def maybe[D <: PivotSize, E](pivot: PivotSize)(
        arr: Array[E]
    ): Option[Multiarray[D, E]] =
      arr.length
        .isDivisibleBy(pivot)
        .option { arr: Multiarray[D, E] }

  /** Typelevel refinement of positive integer as valid ROI diameter (must be even)
    */
  opaque type RoiDiameter <: PositiveInt = PositiveInt

  /** Helpers for working with ROI diameters */
  object RoiDiameter:
    /** Wrap the given value as a ROI diameter if it's legal. */
    def fromInt(z: Int): Either[String, RoiDiameter] =
      PositiveInt
        .either(z)
        .flatMap { n =>
          fromPositiveInteger(n).toRight(
            s"Can't use odd value as ROI diameter: $n"
          )
        }

    /** Try to lift the given positive integer into the ROI diameter type, refining it as even.
      */
    def fromPositiveInteger(n: PositiveInt): Option[RoiDiameter] =
      (n % 2 === 0).option { n: RoiDiameter }
    given showForRoiDiameter: Show[RoiDiameter] = Show.fromToString[RoiDiameter]
  end RoiDiameter

  def getMaxProjectionValues[D <: PivotSize, E: {AdmitsMinimum, ClassTag, Order}](
      d: D
  )(arr: Multiarray[D, E]): Array[E] =
    given maxMonoid: MaximumSeekingMonoid[E] = MaximumSeekingMonoid.instance[E]
    getFlatProjection(d)(arr)

  private def getFlatProjection[D <: PivotSize, E: {ClassTag, Monoid}](
      d: D
  )(arr: Multiarray[D, E]): Array[E] =
    arr.zipWithIndex
      .groupBy((_, i) => i % d)
      .view
      // Not using .combineAll here, since we're working in Array and the method's not available.
      // .mapValues(_.map(_._1).toList.combineAll)
      .mapValues(_.foldLeft(Monoid[E].empty) { case (acc, (e, _)) =>
        acc |+| e
      })
      .toArray
      .sortBy(_._1)
      .map(_._2)
