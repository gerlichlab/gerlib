package at.ac.oeaw.imba.gerlich.gerlib.imaging

/** Context in which an image was captured */
final case class ImagingContext(
    fov: FieldOfViewLike,
    timepoint: ImagingTimepoint,
    channel: ImagingChannel
)
