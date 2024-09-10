package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for types in the
  * [[at.ac.oeaw.imba.gerlich.gerlib.imaging]] package
  */
trait InstancesForImaging:
  /** Decode a field-of-view-like by first trying by integer, then by name. */
  given CellDecoder[FieldOfViewLike] = liftToCellDecoder(FieldOfViewLike.parse)

  /* NB: CellEncoder is invariant, so we have this in addition to encoders for subtypes. */
  given CellEncoder[FieldOfViewLike] =
    CellEncoder.fromSimpleShow[FieldOfViewLike]

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[FieldOfView] = liftToCellDecoder(FieldOfView.parse)

  /** For CSV write, show the FOV just by the numeric value.
    *
    * NB: CellEncoder is invariant, so we have this subtype instance in addition
    * to the instance for the parent.
    */
  given CellEncoder[FieldOfView] with
    override def apply(cell: FieldOfView): String = cell.show_

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[PositionName] = liftToCellDecoder(PositionName.parse)

  /** For CSV write, show the FOV just by the numeric value.
    *
    * NB: CellEncoder is invariant, so we have this subtype instance in addition
    * to the instance for the parent.
    */
  given CellEncoder[PositionName] = CellEncoder.fromSimpleShow[PositionName]

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[ImagingChannel] = liftToCellDecoder(ImagingChannel.parse)

  /** For CSV write, show the channel just by the numeric value. */
  given CellEncoder[ImagingChannel] = CellEncoder.fromSimpleShow[ImagingChannel]

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[ImagingTimepoint] = liftToCellDecoder(
    ImagingTimepoint.parse
  )

  /** For CSV write, show the timepoint just by the numeric value. */
  given CellEncoder[ImagingTimepoint] =
    CellEncoder.fromSimpleShow[ImagingTimepoint]

  given defaultCsvRowDecoderForImagingContext(using
      CellDecoder[FieldOfViewLike],
      CellDecoder[ImagingTimepoint],
      CellDecoder[ImagingChannel]
  ): CsvRowDecoder[ImagingContext, String] = new:
    override def apply(row: RowF[Some, String]): DecoderResult[ImagingContext] =
      val fovNel = ColumnNames.FieldOfViewColumnName.from(row)
      val timeNel = ColumnNames.TimepointColumnName.from(row)
      val channelNel = ColumnNames.ChannelColumnName.from(row)
      (fovNel, timeNel, channelNel)
        .mapN(ImagingContext.apply)
        .toEither
        .leftMap { messages =>
          DecoderError(
            s"Cannot decode imaging context because of ${messages.length} error(s): ${messages.mkString_("; ")}"
          )
        }

  given csvRowEncoderForImagingContext(using
      encFov: CellEncoder[FieldOfViewLike],
      envTime: CellEncoder[ImagingTimepoint],
      encChannel: CellEncoder[ImagingChannel]
  ): CsvRowEncoder[ImagingContext, String] = new:
    override def apply(elem: ImagingContext): RowF[Some, String] =
      val fovRow = ColumnNames.FieldOfViewColumnName.write(elem.fieldOfView)
      val timeRow = ColumnNames.TimepointColumnName.write(elem.timepoint)
      val channelRow = ColumnNames.ChannelColumnName.write(elem.channel)
      fovRow |+| timeRow |+| channelRow
