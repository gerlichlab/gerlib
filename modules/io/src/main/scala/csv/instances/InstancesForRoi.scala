package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import scala.util.NotGiven

import cats.Order
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.ColumnNames.*
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.instances.geometry.given
import at.ac.oeaw.imba.gerlich.gerlib.roi.*
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.*

/** CSV-related typeclass instances for ROI-related data types */
trait InstancesForRoi:
  // Fix the header type slot to be String.
  private type CsvRow = RowF[Some, String]

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
        .mapN { (fov, time, channel, z, y, x, area, intensity) =>
          val center = Centroid.fromPoint(Point3D(x, y, z))
          DetectedSpot(fov, time, channel, center, area, intensity)
        }
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

  /** Decoder for bounding box records from CSV with new headers */
  def newCsvRowDecoderForBoundingBox[C: CellDecoder: Order]
      : CsvRowDecoder[BoundingBox[C], String] = new:
    /** Parse each interval endpoint, then make assemble the intervals. */
    override def apply(row: CsvRow): DecoderResult[BoundingBox[C]] =
      val zNels = row.getPair[C, ZCoordinate[C]](
        zLoColumnNameCamel[C],
        zHiColumnNameCamel[C]
      )
      val yNels = row.getPair[C, YCoordinate[C]](
        yLoColumnNameCamel[C],
        yHiColumnNameCamel[C]
      )
      val xNels = row.getPair[C, XCoordinate[C]](
        xLoColumnNameCamel[C],
        xHiColumnNameCamel[C]
      )
      buildBox(zNels, yNels, xNels).leftMap(row.buildDecoderError)

  /** Decoder for bounding box records from CSV with old headers */
  def oldCsvRowDecoderForBoundingBox[C: CellDecoder: Order]
      : CsvRowDecoder[BoundingBox[C], String] = new:
    /** Parse each interval endpoint, then make assemble the intervals. */
    override def apply(row: CsvRow): DecoderResult[BoundingBox[C]] =
      val zNels = row.getPair[C, ZCoordinate[C]](
        zLoColumnNameSnake[C],
        zHiColumnNameSnake[C]
      )
      val yNels = row.getPair[C, YCoordinate[C]](
        yLoColumnNameSnake[C],
        yHiColumnNameSnake[C]
      )
      val xNels = row.getPair[C, XCoordinate[C]](
        xLoColumnNameSnake[C],
        xHiColumnNameSnake[C]
      )
      buildBox(zNels, yNels, xNels).leftMap(row.buildDecoderError)

  /** Result of an attempt to parse a pair of interval endpoints */
  private type MaybeEndpoints[A, C <: Coordinate[A]] =
    (ValidatedNel[String, C], ValidatedNel[String, C])

  /* Syntax enrichments for parsing data from CSV row */
  extension (row: CsvRow)
    /* Try to parse a pair of interval endpoints from the row. */
    private def getPair[A, C <: Coordinate[A]: CellDecoder: [C] =>> NotGiven[
      C =:= Coordinate[A]
    ]](
        keyLo: ColumnName[C],
        keyHi: ColumnName[C]
    ): MaybeEndpoints[A, C] =
      keyLo.from(row) -> keyHi.from(row)
    /* Aggregative error messages into single decoder error. */
    private def buildDecoderError(
        messages: NonEmptyList[String]
    ): DecoderError =
      DecoderError(
        s"${messages.size} error(s) decoding bounding box from row ($row): ${messages.mkString_("; ")}"
      )

  /** Attempt to construct a bounding box from the given pairs of endpoints. */
  private def buildBox[C: Order](
      zBoundNels: MaybeEndpoints[C, ZCoordinate[C]],
      yBoundNels: MaybeEndpoints[C, YCoordinate[C]],
      xBoundNels: MaybeEndpoints[C, XCoordinate[C]]
  ): Either[NonEmptyList[String], BoundingBox[C]] =
    (
      zBoundNels._1,
      zBoundNels._2,
      yBoundNels._1,
      yBoundNels._2,
      xBoundNels._1,
      xBoundNels._2
    ).tupled.toEither
      .flatMap { (zLo, zHi, yLo, yHi, xLo, xHi) =>
        val zIntervalNel =
          BoundingBox.Interval.fromTuple(zLo -> zHi).toValidatedNel
        val yIntervalNel =
          BoundingBox.Interval.fromTuple(yLo -> yHi).toValidatedNel
        val xIntervalNel =
          BoundingBox.Interval.fromTuple(xLo -> xHi).toValidatedNel
        (xIntervalNel, yIntervalNel, zIntervalNel)
          .mapN(BoundingBox.apply)
          .toEither
      }
end InstancesForRoi
