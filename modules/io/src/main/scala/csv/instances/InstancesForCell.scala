package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.cell.*
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

trait InstancesForCell:
  given cellDecoderForNuclearDesignation: CellDecoder[NuclearDesignation] =
    liftToCellDecoder(NuclearDesignation.parse)

  given cellEncoderForNuclearDesignation: CellEncoder[NuclearDesignation] with
    override def apply(cell: NuclearDesignation): String = cell.show_
