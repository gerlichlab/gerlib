package at.ac.oeaw.imba.gerlich.gerlib.roi

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.{Area, MeanIntensity}

/** Bundle of imaging context, location, and measurements of a detected FISH
  * spot
  */
final case class DetectedSpot[C](
    context: ImagingContext,
    centroid: Centroid[C],
    area: Area,
    intensity: MeanIntensity
):
  def fieldOfView: FieldOfViewLike = context.fieldOfView

  def timepoint: ImagingTimepoint = context.timepoint

  def channel: ImagingChannel = context.channel

  def centerZ: ZCoordinate[C] = centroid.z

  def centerY: YCoordinate[C] = centroid.y

  def centerX: XCoordinate[C] = centroid.x
end DetectedSpot
