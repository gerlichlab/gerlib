package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

// Seems to be needed to get the Order[Int :| Nonnegative] instance
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt.given

/** Semantic wrapper around value representing 0-based imaging channel */
final case class ImagingChannel(get: NonnegativeInt) derives Order, Show

/** Helpers for working with imaging channels */
object ImagingChannel:
    /** Wrap the given value as an imaging channel, if it's valid as one. */
    def parse: String => Either[String, ImagingChannel] = 
        parseThroughNonnegativeInt("ImagingChannel")(ImagingChannel.apply)
end ImagingChannel
