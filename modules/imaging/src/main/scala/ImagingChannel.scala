package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.cats.given_Order_:| // for derivation of Order for wrapper type
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.numeric.parseThroughNonnegativeInt
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Semantic wrapper around value representing 0-based imaging channel */
final case class ImagingChannel(private[gerlib] get: Int :| Not[Negative]) derives Order

/** Helpers for working with imaging channels */
object ImagingChannel:
  /** Wrap the given value as an imaging channel, if it's valid as one. */
  def parse: String => Either[String, ImagingChannel] =
    parseThroughNonnegativeInt("ImagingChannel")(ImagingChannel.apply)
end ImagingChannel
