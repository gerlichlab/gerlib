package at.ac.oeaw.imba.gerlich.gerlib.roi

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.{Area, MeanIntensity}

/** Bundle of imaging context, location, and measurements of a detected FISH
  * spot
  */
final case class DetectedSpot[C](
    fieldOfView: FieldOfViewLike,
    timepoint: ImagingTimepoint,
    channel: ImagingChannel,
    centroid: Centroid[C], 
    area: Area,
    intensity: MeanIntensity
):
  def centerZ: ZCoordinate[C] = centroid.z
  def centerY: YCoordinate[C] = centroid.y
  def centerX: XCoordinate[C] = centroid.x

  /** Return the imaging experiment context in which this spot was detected.
    */
  def context: ImagingContext =
    ImagingContext(fieldOfView, timepoint, channel)
end DetectedSpot
