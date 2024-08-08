package at.ac.oeaw.imba.gerlich.gerlib.io
package csv

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.NuclearDesignation

/** A name corresponding to a CSV file column */
trait ColumnNameLike[A]:
  /** The raw column name */
  def value: String

  /** Try to read a value of type {@code A} from value at key {@code value}. */
  def pullFrom(row: RowF[Some, String])(using
      CellDecoder[A]
  ): DecoderResult[A] =
    row.as[A](value)
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
  /** Columns for encoding attribution/designation of nucleus */
  val NewNucleusDesignationColumnName =
    ColumnName[NuclearDesignation]("nucleusNumber")
  val OldNucleusDesignationColumnName =
    ColumnName[NuclearDesignation]("nuc_label")
