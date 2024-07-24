package at.ac.oeaw.imba.gerlich.gerlib.imaging

/** Context in which an image was captured */
final case class ImagingContext(
    fov: FieldOfView,
    timepoint: ImagingTimepoint,
    channel: ImagingChannel
)
