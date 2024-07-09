package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import cats.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given

/** Semantic wrapper around value representing 0-based imaging channel */
final case class ImagingChannel(get: NonnegativeInt) derives Order

/** Helpers for working with imaging channels */
object ImagingChannel:
  /** Wrap the given value as an imaging channel, if it's valid as one. */
  def parse: String => Either[String, ImagingChannel] =
    parseThroughNonnegativeInt("ImagingChannel")(ImagingChannel.apply)
  given showForImagingChannel: Show[ImagingChannel] = Show.show(_.get.show)
end ImagingChannel
