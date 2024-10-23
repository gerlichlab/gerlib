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

  private type NamedFieldReader[T] = (ColumnNameLike[T], CellDecoder[T])

  extension [T](reader: NamedFieldReader[T])
    def validatedNel(row: CsvRow): ValidatedNel[String, T] =
      reader._1.from(row)(using reader._2)

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

  given csvRowDecoderForDetectedSpotFromNamedFieldReaders[C](using
      fovReader: NamedFieldReader[FieldOfViewLike],
      timeReader: NamedFieldReader[ImagingTimepoint],
      channelReader: NamedFieldReader[ImagingChannel],
      zReader: NamedFieldReader[ZCoordinate[C]],
      yReader: NamedFieldReader[YCoordinate[C]],
      xReader: NamedFieldReader[XCoordinate[C]],
      areaReader: NamedFieldReader[Area],
      intensityReader: NamedFieldReader[MeanIntensity]
  ): CsvRowDecoder[DetectedSpot[C], String] with
    override def apply(
        row: CsvRow
    ): DecoderResult[DetectedSpot[C]] =
      val fovNel = fovReader.validatedNel(row)
      val timeNel = timeReader.validatedNel(row)
      val channelNel = channelReader.validatedNel(row)
      val zNel = zReader.validatedNel(row)
      val yNel = yReader.validatedNel(row)
      val xNel = xReader.validatedNel(row)
      val areaNel = areaReader.validatedNel(row)
      val intensityNel = intensityReader.validatedNel(row)
      (fovNel, timeNel, channelNel, zNel, yNel, xNel, areaNel, intensityNel)
        .mapN { (fov, time, channel, z, y, x, area, intensity) =>
          val center = Centroid.fromPoint(Point3D(x, y, z))
          DetectedSpot(
            ImagingContext(fov, time, channel),
            center,
            area,
            intensity
          )
        }
        .toEither
        .leftMap { es =>
          DecoderError(
            s"${es.size} error(s) decoding detected spot: ${es.mkString_("; ")}"
          )
        }

  /** The default given instance uses all the default column names. */
  given defaultCsvRowDecoderForDetectedSpot[C](using
      CellDecoder[FieldOfViewLike],
      CellDecoder[ImagingTimepoint],
      CellDecoder[ImagingChannel],
      CellDecoder[ZCoordinate[C]],
      CellDecoder[YCoordinate[C]],
      CellDecoder[XCoordinate[C]]
  ): CsvRowDecoder[DetectedSpot[C], String] =
    getCsvRowDecoderForDetectedSpot()

  /** Parse the detected spot from CSV field-by-field. */
  def getCsvRowDecoderForDetectedSpot[C](
      fovCol: ColumnNameLike[FieldOfViewLike] = FieldOfViewColumnName,
      timeCol: ColumnNameLike[ImagingTimepoint] = TimepointColumnName,
      channelCol: ColumnNameLike[ImagingChannel] = SpotChannelColumnName,
      zCol: ColumnNameLike[ZCoordinate[C]] = zCenterColumnName[C],
      yCol: ColumnNameLike[YCoordinate[C]] = yCenterColumnName[C],
      xCol: ColumnNameLike[XCoordinate[C]] = xCenterColumnName[C],
      areaCol: ColumnNameLike[Area] = AreaColumnName,
      intensityCol: ColumnNameLike[MeanIntensity] = IntensityColumnName
  )(using
      decFov: CellDecoder[FieldOfViewLike],
      decTime: CellDecoder[ImagingTimepoint],
      decChannel: CellDecoder[ImagingChannel],
      decZ: CellDecoder[ZCoordinate[C]],
      decY: CellDecoder[YCoordinate[C]],
      decX: CellDecoder[XCoordinate[C]]
  ): CsvRowDecoder[DetectedSpot[C], String] = new:
    override def apply(
        row: CsvRow
    ): DecoderResult[DetectedSpot[C]] =
      csvRowDecoderForDetectedSpotFromNamedFieldReaders(using
        fovCol -> decFov,
        timeCol -> decTime,
        channelCol -> decChannel,
        zCol -> decZ,
        yCol -> decY,
        xCol -> decX,
        areaCol -> summon[CellDecoder[Area]],
        intensityCol -> cellDecoderForMeanIntensity
      )(row)

  given defaultCsvRowEncoderForDetectedSpot[C](using
      CellEncoder[FieldOfViewLike],
      CellEncoder[ImagingTimepoint],
      CellEncoder[ImagingChannel],
      CellEncoder[ZCoordinate[C]],
      CellEncoder[YCoordinate[C]],
      CellEncoder[XCoordinate[C]]
  ): CsvRowEncoder[DetectedSpot[C], String] = getCsvRowEncoderForDetectedSpot()

  /** Encode the given spot field-by-field, using the column/key/field names defined in this object.
    */
  def getCsvRowEncoderForDetectedSpot[C](
      fovCol: ColumnNameLike[FieldOfViewLike] = FieldOfViewColumnName,
      timeCol: ColumnNameLike[ImagingTimepoint] = TimepointColumnName,
      channelCol: ColumnNameLike[ImagingChannel] = SpotChannelColumnName,
      zCol: ColumnNameLike[ZCoordinate[C]] = zCenterColumnName[C],
      yCol: ColumnNameLike[YCoordinate[C]] = yCenterColumnName[C],
      xCol: ColumnNameLike[XCoordinate[C]] = xCenterColumnName[C],
      areaCol: ColumnNameLike[Area] = AreaColumnName,
      intensityCol: ColumnNameLike[MeanIntensity] = IntensityColumnName
  )(using
      encFov: CellEncoder[FieldOfViewLike],
      encTime: CellEncoder[ImagingTimepoint],
      encCh: CellEncoder[ImagingChannel],
      encZ: CellEncoder[ZCoordinate[C]],
      encY: CellEncoder[YCoordinate[C]],
      encX: CellEncoder[XCoordinate[C]]
  ): CsvRowEncoder[DetectedSpot[C], String] = new:
    override def apply(elem: DetectedSpot[C]): CsvRow =
      val kvs = NonEmptyList.of(
        fovCol -> encFov(elem.fieldOfView),
        timeCol -> encTime(elem.timepoint),
        channelCol -> encCh(elem.channel),
        zCol -> encZ(elem.centerZ),
        yCol -> encY(elem.centerY),
        xCol -> encX(elem.centerX),
        areaCol -> summon[CellEncoder[Area]](elem.area),
        intensityCol -> cellEncoderForMeanIntensity(elem.intensity)
      )
      val (headers, textFields) = kvs.unzip
      RowF(values = textFields, headers = Some(headers.map(_.value)))

  /** Decoder for bounding box records from CSV with new headers */
  given csvRowDecoderForBoundingBox[C: CellDecoder: Order]: CsvRowDecoder[BoundingBox[C], String]
  with
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

  given csvRowEncoderForBoundingBox[C](using
      encZ: CellEncoder[ZCoordinate[C]],
      encY: CellEncoder[YCoordinate[C]],
      encX: CellEncoder[XCoordinate[C]]
  ): CsvRowEncoder[BoundingBox[C], String] = new:
    override def apply(box: BoundingBox[C]): CsvRow =
      val kvs: NonEmptyList[(ColumnName[?], String)] = NonEmptyList.of(
        zLoColumnNameCamel -> encZ(box.sideZ.lo),
        zHiColumnNameCamel -> encZ(box.sideZ.hi),
        yLoColumnNameCamel -> encY(box.sideY.lo),
        yHiColumnNameCamel -> encY(box.sideY.hi),
        xLoColumnNameCamel -> encX(box.sideX.lo),
        xHiColumnNameCamel -> encX(box.sideX.hi)
      )
      val (names, values) = kvs.unzip
      RowF(values = values, headers = Some(names.map(_.value)))

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
