package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

trait InstancesForImaging:
  given cellDecoderForFieldOfView: CellDecoder[FieldOfView] =
    liftToCellDecoder(FieldOfView.parse)

  /** For CSV write, show the FOV just by the numeric value. */
  given cellEncoderForFieldOfView: CellEncoder[FieldOfView] with
    override def apply(cell: FieldOfView): String = cell.show_

  given cellDecoderForImagingChannel: CellDecoder[ImagingChannel] =
    liftToCellDecoder(ImagingChannel.parse)

  /** For CSV write, show the channel just by the numeric value. */
  given cellEncoderForImagingChannel: CellEncoder[ImagingChannel] with
    override def apply(cell: ImagingChannel): String = cell.show_

  given cellDecoderForImagingTimepoint: CellDecoder[ImagingTimepoint] =
    liftToCellDecoder(ImagingTimepoint.parse)

  /** For CSV write, show the timepoint just by the numeric value. */
  given cellEncoderForImagingTimepoint: CellEncoder[ImagingTimepoint] with
    override def apply(cell: ImagingTimepoint): String = cell.show_

  private[io] def nuclearDesignationKey: String = "nuclusNumber"

  def singletonRowDecoderForNuclearDesignation(using
      CellDecoder[NuclearDesignation]
  ): CsvRowDecoder[NuclearDesignation, String] =
    getCsvRowDecoderForSingleton(nuclearDesignationKey)

  def singletonRowEncoderForNuclearDesignation(using
      CellEncoder[NuclearDesignation]
  ): CsvRowEncoder[NuclearDesignation, String] =
    getCsvRowEncoderForSingleton(nuclearDesignationKey)
