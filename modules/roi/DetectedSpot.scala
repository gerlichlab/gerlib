package at.ac.oeaw.imba.gerlich.gerlib.roi

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.{Area, MeanIntensity}

/** Bundle of imaging context, location, and measurements of a detected FISH
  * spot
  */
final case class DetectedSpot[C](
    fieldOfView: FieldOfView,
    timepoint: ImagingTimepoint,
    channel: ImagingChannel,
    centerZ: ZCoordinate[C],
    centerY: YCoordinate[C],
    centerX: XCoordinate[C],
    area: Area,
    intensity: MeanIntensity
):
  /** Bundle the spatial information of this spot instance into a centroid
    * value.
    */
  def centroid: Centroid[C] =
    Centroid.fromPoint(Point3D(centerX, centerY, centerZ))

  /** Return the imaging experiment context in which this spot was detected.
    */
  def context: ImagingContext =
    ImagingContext(fieldOfView, timepoint, channel)
end DetectedSpot
