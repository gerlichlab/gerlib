package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Semantic wrapper around value representing 0-based imaging channel */
final case class ImagingChannel(private[gerlib] get: NonnegativeInt)
    derives Order

/** Helpers for working with imaging channels */
object ImagingChannel:
  /** Wrap the given value as an imaging channel, if it's valid as one. */
  def parse: String => Either[String, ImagingChannel] =
    parseThroughNonnegativeInt("ImagingChannel")(ImagingChannel.apply)

  given SimpleShow[ImagingChannel] = SimpleShow.instance(_.get.show_)
end ImagingChannel
