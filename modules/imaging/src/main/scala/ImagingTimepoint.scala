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
  def fromInt = NonnegativeInt.either.map(_.map(ImagingTimepoint.apply))

  /** Wrap the given value as an imaging timepoint, if it's valid as one. */
  def parse: String => Either[String, ImagingTimepoint] =
    parseThroughNonnegativeInt("ImagingTimepoint")(ImagingTimepoint.apply)
end ImagingTimepoint
