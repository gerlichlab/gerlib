package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.cats.given_Order_:| // for derivation of Order for wrapper type
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

/** Semantic wrapper around value representing 0-based imaging timepoint */
final case class ImagingTimepoint(private[gerlib] get: Int :| Not[Negative]) derives Order

/** Helpers for working with imaging timepoints */
object ImagingTimepoint:
  /** Attempt to create a timepoint from an integer, first refining as nonnegative integer .
    */
  def fromInt: Int => Either[String, ImagingTimepoint] =
    NonnegativeInt.either.map(_.map(ImagingTimepoint.apply))

  /** Wrap the given value as an imaging timepoint, if it's valid as one. */
  def parse: String => Either[String, ImagingTimepoint] =
    parseThroughNonnegativeInt("ImagingTimepoint")(ImagingTimepoint.apply)

  /** Attempt to create a new imaging timepoint by shifting one by the given increment.
    */
  def shift(t: ImagingTimepoint)(delta: Int): Either[String, ImagingTimepoint] =
    fromInt(t.get + delta)

  /** Attempt to create a new imaging timepoint by shifting one by the given increment.
    */
  def shift(t1: ImagingTimepoint)(
      t2: ImagingTimepoint
  ): Either[String, ImagingTimepoint] = shift(t1)(t2.get)

  /** Lift the given integer to nonnegative, then wrap as an imaging timepoint.
    */
  def unsafeLift = (i: Int) =>
    NonnegativeInt.option(i) match {
    case None    => throw IllegalRefinement(i, s"Illegal value as imaging timepoint: $i")
    case Some(t) => ImagingTimepoint(t)
    }

  extension (t: ImagingTimepoint)
    /** Attempt to create a new imaging timepoint by shifting one by the given increment.
      */
    def unsafeShift(delta: ImagingTimepoint): ImagingTimepoint = unsafeShift(
      delta.get
    )

    /** Attempt to create a new imaging timepoint by shifting one by the given increment.
      */
    def unsafeShift(delta: Int): ImagingTimepoint = unsafeLift(t.get + delta)
end ImagingTimepoint
