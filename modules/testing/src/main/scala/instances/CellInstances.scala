package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.syntax.all.*
import org.scalacheck.*
import at.ac.oeaw.imba.gerlich.gerlib.cell.NuclearDesignation
import at.ac.oeaw.imba.gerlich.gerlib.cell.NucleusNumber
import at.ac.oeaw.imba.gerlich.gerlib.cell.OutsideNucleus
import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt
import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.all.given
import at.ac.oeaw.imba.gerlich.gerlib.testing.syntax.scalacheck.*

/** Testing-related typeclass instances for cell-related data types */
trait CellInstances:
  given arbitraryForNucleusNumber(using
      arbRaw: Arbitrary[PositiveInt]
  ): Arbitrary[NucleusNumber] =
    arbRaw.map(NucleusNumber.apply)

  given arbitraryForNuclearDesignation(using
      Arbitrary[NucleusNumber]
  ): Arbitrary[NuclearDesignation] =
    Gen
      .option(Arbitrary.arbitrary[NucleusNumber])
      .map(_.getOrElse(OutsideNucleus))
      .toArbitrary
end CellInstances
