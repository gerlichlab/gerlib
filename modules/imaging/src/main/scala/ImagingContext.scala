package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.Order
import cats.derived.*

/** Context in which an image was captured */
final case class ImagingContext(
    fieldOfView: FieldOfViewLike,
    timepoint: ImagingTimepoint,
    channel: ImagingChannel
) derives Order
