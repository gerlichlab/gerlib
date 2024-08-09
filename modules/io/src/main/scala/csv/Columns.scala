package at.ac.oeaw.imba.gerlich.gerlib.io
package csv

import cats.data.ValidatedNel
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.NuclearDesignation
import at.ac.oeaw.imba.gerlich.gerlib.geometry.{
  XCoordinate,
  YCoordinate,
  ZCoordinate
}
import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingTimepoint
import at.ac.oeaw.imba.gerlich.gerlib.imaging.FieldOfViewLike
import at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingChannel
import at.ac.oeaw.imba.gerlich.gerlib.roi.measurement.{Area, MeanIntensity}

/** A name corresponding to a CSV file column */
trait ColumnNameLike[A]:
  /** The raw column name */
  def value: String

  /** Try to read a value of type {@code A} from value at key {@code value}. */
  def from(row: RowF[Some, String])(using
      CellDecoder[A]
  ): ValidatedNel[String, A] =
    row.as[A](value).leftMap(_.getMessage).toValidatedNel
end ColumnNameLike

/** The name of a column from which an {@code A} is extracted.
  *
  * @tparam A
  *   The type of value stored in the column with this name
  * @param value
  *   The raw name of a CSV column
  */
final case class ColumnName[A](value: String) extends ColumnNameLike[A]

/** Collection of names of critical columns from which to parse data */
object ColumnNames:
  /** New column name for designation of a record's nucleus */
  val NewNucleusDesignationColumnName =
    ColumnName[NuclearDesignation]("nucleusNumber")

  /** Original column name for designation of a record's nucleus */
  val OldNucleusDesignationColumnName =
    ColumnName[NuclearDesignation]("nuc_label")

  /** Key for field of view */
  val FieldOfViewColumnName = ColumnName[FieldOfViewLike]("fov")

  /** Key for field of imaging timepont */
  val TimepointColumnName = ColumnName[ImagingTimepoint]("time")

  /** Key for field of imaging channel */
  val ChannelColumnName = ColumnName[ImagingChannel]("roi_channel")

  /** Key for z-coordinate of spot centroid */
  def zCenterColumnName[C] = ColumnName[ZCoordinate[C]]("zc")

  /** Key for x-coordinate of spot centroid */
  def yCenterColumnName[C] = ColumnName[YCoordinate[C]]("yc")

  /** Key for x-coordinate of spot centroid */
  def xCenterColumnName[C] = ColumnName[XCoordinate[C]]("xc")

  /** Key for area of 2D detected spot */
  val AreaColumnName = ColumnName[Area]("area")

  /** Key for the mean pixel intensity value within a detected spot */
  val IntensityColumnName = ColumnName[MeanIntensity]("intensity_mean")

  /* Related to bounding box */

  /** Camel case column name for lower bound of z interval for a bounding box */
  def zLoColumnNameCamel[C] = ColumnName[ZCoordinate[C]]("zMin")

  /** Camel case column name for upper bound of z interval for a bounding box */
  def zHiColumnNameCamel[C] = ColumnName[ZCoordinate[C]]("zMax")

  /** Camel case column name for lower bound of y interval for a bounding box */
  def yLoColumnNameCamel[C] = ColumnName[YCoordinate[C]]("yMin")

  /** Camel case column name for upper bound of y interval for a bounding box */
  def yHiColumnNameCamel[C] = ColumnName[YCoordinate[C]]("yMax")

  /** Camel case column name for lower bound of x interval for a bounding box */
  def xLoColumnNameCamel[C] = ColumnName[XCoordinate[C]]("xMin")

  /** Camel case column name for upper bound of x interval for a bounding box */
  def xHiColumnNameCamel[C] = ColumnName[XCoordinate[C]]("xMax")

  /** Snake case column name for lower bound of z interval for a bounding box */
  def zLoColumnNameSnake[C] = ColumnName[ZCoordinate[C]]("z_min")

  /** Snake case column name for upper bound of z interval for a bounding box */
  def zHiColumnNameSnake[C] = ColumnName[ZCoordinate[C]]("z_max")

  /** Snake case column name for lower bound of y interval for a bounding box */
  def yLoColumnNameSnake[C] = ColumnName[YCoordinate[C]]("y_min")

  /** Snake case column name for upper bound of y interval for a bounding box */
  def yHiColumnNameSnake[C] = ColumnName[YCoordinate[C]]("y_max")

  /** Snake case column name for lower bound of x interval for a bounding box */
  def xLoColumnNameSnake[C] = ColumnName[XCoordinate[C]]("x_min")

  /** Snake case column name for upper bound of x interval for a bounding box */
  def xHiColumnNameSnake[C] = ColumnName[XCoordinate[C]]("x_max")
end ColumnNames
