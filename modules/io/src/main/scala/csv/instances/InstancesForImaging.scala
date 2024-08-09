package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.imaging.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for types in the
  * [[at.ac.oeaw.imba.gerlich.gerlib.imaging]] package
  */
trait InstancesForImaging:
  /** Decode a field-of-view-like by first trying by integer, then by name. */
  given CellDecoder[FieldOfViewLike] = liftToCellDecoder(FieldOfViewLike.parse)

  given CellEncoder[FieldOfViewLike] with
    override def apply(fovLike: FieldOfViewLike): String = fovLike.show_

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[FieldOfView] = liftToCellDecoder(FieldOfView.parse)

  /** For CSV write, show the FOV just by the numeric value. */
  given CellEncoder[FieldOfView] with
    override def apply(cell: FieldOfView): String = cell.show_

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[PositionName] = liftToCellDecoder(PositionName.parse)

  /** For CSV write, show the FOV just by the numeric value. */
  given CellEncoder[PositionName] with
    override def apply(cell: PositionName): String = cell.show_

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[ImagingChannel] = liftToCellDecoder(ImagingChannel.parse)

  /** For CSV write, show the channel just by the numeric value. */
  given CellEncoder[ImagingChannel] with
    override def apply(cell: ImagingChannel): String = cell.show_

  /** Decode a CSV field/cell by using the companion object's parse function. */
  given CellDecoder[ImagingTimepoint] = liftToCellDecoder(
    ImagingTimepoint.parse
  )

  /** For CSV write, show the timepoint just by the numeric value. */
  given CellEncoder[ImagingTimepoint] with
    override def apply(cell: ImagingTimepoint): String = cell.show_
