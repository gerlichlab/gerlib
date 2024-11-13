package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.*
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances and helper functions for cell-related data types */
trait InstancesForCell:
  given CellDecoder[NucleusNumber] = liftToCellDecoder(NucleusNumber.parse)

  given cellDecoderForNuclearDesignation: CellDecoder[NuclearDesignation] =
    liftToCellDecoder(NuclearDesignation.parse)

  given cellEncoderForNuclearDesignation: CellEncoder[NuclearDesignation] with
    override def apply(cell: NuclearDesignation): String = cell.show_

  def getCsvRowDecoderForNuclearDesignation(
      key: ColumnNameLike[NuclearDesignation] = ColumnNames.NucleusDesignationColumnName
  )(using
      CellDecoder[NuclearDesignation]
  ): CsvRowDecoder[NuclearDesignation, String] =
    getCsvRowDecoderForSingleton(key)

  def getCsvRowEncoderForNuclearDesignation(
      key: ColumnNameLike[NuclearDesignation] = ColumnNames.NucleusDesignationColumnName
  )(using
      CellEncoder[NuclearDesignation]
  ): CsvRowEncoder[NuclearDesignation, String] =
    getCsvRowEncoderForSingleton(key)
