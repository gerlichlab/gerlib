package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.*

trait InstancesForRoi:

  /** The keys / column names of input files which correspond to the fields of
    * the detected spot case class
    */
  private enum Key(val get: String):
    /** Key for field of view */
    case FieldOfView extends Key("fov")

    /** Key for field of imaging timepont */
    case Timepoint extends Key("time")

    /** Key for field of imaging channel */
    case Channel extends Key("roi_channel")

    /** Key for z-coordinate of spot centroid */
    case CenterZ extends Key("zc")

    /** Key for y-coordinate of spot centroid */
    case CenterY extends Key("yc")

    /** Key for x-cordinate of spot centroid */
    case CenterX extends Key("xc")

    /** Key for area of 2D detected spot */
    case Area extends Key("area")

    /** Key for the mean pixel intensity value within a detected spot */
    case Intensity extends Key("intensity_mean")
  end Key

  given cellDecoderForArea: CellDecoder[Area] =
    liftToCellDecoder(Area.parse)

  given cellEncoderForArea(using
      enc: CellEncoder[Double]
  ): CellEncoder[Area] = enc.contramap(_.toDouble)

  given cellDecoderForMeanIntensity: CellDecoder[MeanIntensity] =
    liftToCellDecoder(MeanIntensity.parse)

  given cellEncoderForMeanIntensity(using
      enc: CellEncoder[Double]
  ): CellEncoder[MeanIntensity] = enc.contramap(_.toDouble)

  /** Parse the detected spot from CSV field-by-field. */
  given csvRowDecoderForDetectedSpot[C](using
      CellDecoder[FieldOfViewLike],
      CellDecoder[ImagingTimepoint],
      CellDecoder[ImagingChannel],
      CellDecoder[ZCoordinate[C]],
      CellDecoder[YCoordinate[C]],
      CellDecoder[XCoordinate[C]]
  ): CsvRowDecoder[DetectedSpot[C], String] with
    override def apply(
        row: RowF[Some, String]
    ): DecoderResult[DetectedSpot[C]] =
      val fovNel = row.as[FieldOfViewLike](Key.FieldOfView.get).toValidatedNel
      val timeNel = row.as[ImagingTimepoint](Key.Timepoint.get).toValidatedNel
      val channelNel = row.as[ImagingChannel](Key.Channel.get).toValidatedNel
      val zNel = row.as[ZCoordinate[C]](Key.CenterZ.get).toValidatedNel
      val yNel = row.as[YCoordinate[C]](Key.CenterY.get).toValidatedNel
      val xNel = row.as[XCoordinate[C]](Key.CenterX.get).toValidatedNel
      val areaNel = row.as[Area](Key.Area.get).toValidatedNel

      // Disambiguate b/w NonnegativeReal and MeanIntensity, which are the same here in this scope.
      given decMeanIntensity: CellDecoder[MeanIntensity] =
        cellDecoderForMeanIntensity

      val intensityNel =
        row.as[MeanIntensity](Key.Intensity.get).toValidatedNel
      (fovNel, timeNel, channelNel, zNel, yNel, xNel, areaNel, intensityNel)
        .mapN(DetectedSpot.apply[C])
        .toEither
        .leftMap { es =>
          val msg =
            s"${es.size} error(s) decoding detected spot from row ($row): ${es.map(_.getMessage).mkString_("; ")}"
          DecoderError(msg)
        }

  /** Encode the given spot field-by-field, using the column/key/field names
    * defined in this object.
    */
  given csvRowEncoderForDetectedSpot[C: CellEncoder](using
      encFov: CellEncoder[FieldOfViewLike],
      encTime: CellEncoder[ImagingTimepoint],
      encCh: CellEncoder[ImagingChannel],
      encZ: CellEncoder[ZCoordinate[C]],
      encY: CellEncoder[YCoordinate[C]],
      encX: CellEncoder[XCoordinate[C]]
  ): CsvRowEncoder[DetectedSpot[C], String] with
    override def apply(elem: DetectedSpot[C]): RowF[Some, String] =
      val kvs = NonEmptyList.of(
        Key.FieldOfView.get -> encFov(elem.fieldOfView),
        Key.Timepoint.get -> encTime(elem.timepoint),
        Key.Channel.get -> encCh(elem.channel),
        Key.CenterZ.get -> encZ(elem.centerZ),
        Key.CenterY.get -> encY(elem.centerY),
        Key.CenterX.get -> encX(elem.centerX),
        Key.Area.get -> summon[CellEncoder[Area]](elem.area),
        Key.Intensity.get -> cellEncoderForMeanIntensity(
          elem.intensity
        )
      )
      val (headers, textFields) = kvs.unzip
      RowF(textFields, Some(headers))
end InstancesForRoi
