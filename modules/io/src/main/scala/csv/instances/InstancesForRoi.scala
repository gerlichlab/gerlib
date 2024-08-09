package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.ColumnNames.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.*

trait InstancesForRoi:

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
      val fovNel = FieldOfViewColumnName.from(row)
      val timeNel = TimepointColumnName.from(row)
      val channelNel = ChannelColumnName.from(row)
      val zNel = zCenterColumnName[C].from(row)
      val yNel = yCenterColumnName[C].from(row)
      val xNel = xCenterColumnName[C].from(row)
      val areaNel = AreaColumnName.from(row)

      // Disambiguate b/w NonnegativeReal and MeanIntensity, which are the same here in this scope.
      given decMeanIntensity: CellDecoder[MeanIntensity] =
        cellDecoderForMeanIntensity

      val intensityNel = IntensityColumnName.from(row)
      (fovNel, timeNel, channelNel, zNel, yNel, xNel, areaNel, intensityNel)
        .mapN(DetectedSpot.apply[C])
        .toEither
        .leftMap { es =>
          DecoderError(
            s"${es.size} error(s) decoding detected spot from row ($row): ${es.mkString_("; ")}"
          )
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
        FieldOfViewColumnName -> encFov(elem.fieldOfView),
        TimepointColumnName -> encTime(elem.timepoint),
        ChannelColumnName -> encCh(elem.channel),
        zCenterColumnName[C] -> encZ(elem.centerZ),
        yCenterColumnName[C] -> encY(elem.centerY),
        xCenterColumnName[C] -> encX(elem.centerX),
        AreaColumnName -> summon[CellEncoder[Area]](elem.area),
        IntensityColumnName -> cellEncoderForMeanIntensity(
          elem.intensity
        )
      )
      val (headers, textFields) = kvs.unzip
      RowF(textFields, Some(headers.map(_.value)))
end InstancesForRoi
