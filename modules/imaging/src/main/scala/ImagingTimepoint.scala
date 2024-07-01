package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import cats.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

// Seems to be needed to get the Order[Int :| Nonnegative] instance
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt.given

/** Semantic wrapper around value representing 0-based imaging timepoint */
final case class ImagingTimepoint(get: NonnegativeInt) derives Order

/** Helpers for working with imaging timepoints */
object ImagingTimepoint:
    /** Wrap the given value as an imaging timepoint, if it's valid as one. */
    def parse: String => Either[String, ImagingTimepoint] = 
        parseThroughNonnegativeInt("ImagingTimepoint")(ImagingTimepoint.apply)
    given showForImagingTimepoint: Show[ImagingTimepoint] = Show.show(_.get.show)
end ImagingTimepoint