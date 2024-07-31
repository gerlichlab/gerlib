package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given

/** Semantic wrapper around value representing 0-based imaging timepoint */
final case class ImagingTimepoint(private[gerlib] get: NonnegativeInt)
    derives Order

/** Helpers for working with imaging timepoints */
object ImagingTimepoint:
  /** Attempt to create a timepoint from an integer, first refining through
    * {@code NonnegativeInt} .
    */
  def fromInt: Int => Either[String, ImagingTimepoint] =
    NonnegativeInt.either.map(_.map(ImagingTimepoint.apply))

  /** Wrap the given value as an imaging timepoint, if it's valid as one. */
  def parse: String => Either[String, ImagingTimepoint] =
    parseThroughNonnegativeInt("ImagingTimepoint")(ImagingTimepoint.apply)

  /** Attempt to create a new imaging timepoint by shifting one by the given
    * increment.
    */
  def shift(t: ImagingTimepoint)(delta: Int): Either[String, ImagingTimepoint] =
    fromInt(t.get + delta)

  /** Attempt to create a new imaging timepoint by shifting one by the given
    * increment.
    */
  def shift(t1: ImagingTimepoint)(
      t2: ImagingTimepoint
  ): Either[String, ImagingTimepoint] = shift(t1)(t2.get)

  /** Attempt to create a new imaging timepoint by shifting one by the given
    * increment.
    */
  def unsafeShift(t0: ImagingTimepoint)(delta: Int): ImagingTimepoint = 
    unsafeLift(t0.get + delta)
  
  /** Attempt to create a new imaging timepoint by shifting one by the given
    * increment.
    */
  def unsafeShift(t1: ImagingTimepoint)(t2: ImagingTimepoint): ImagingTimepoint = 
    unsafeShift(t1)(t2.get)
  
  def unsafeLift = ImagingTimepoint.apply `compose` NonnegativeInt.unsafe
end ImagingTimepoint
