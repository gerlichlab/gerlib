package at.ac.oeaw.imba.gerlich.gerlib.cell

import cats.*
import cats.instances.int.*
import cats.derived.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.positiveInt.given

/** Designation of whether something's in a cell nucleus or not */
sealed trait NuclearDesignation

/** Something that's not in a cell nucleus */
case object OutsideNucleus extends NuclearDesignation

/** A natural number with which to label / identify a nucleus in a field of view
  */
final case class NucleusNumber(get: PositiveInt) extends NuclearDesignation
    derives Order

object NucleusNumber:
  given Show[NucleusNumber] = Show.show(_.get.show)

/** Helpers for working with nuclei number labels */
object NuclearDesignation:
  /** Attempt to read the given text as a nucleus number. */
  def parse(s: String): Either[String, NuclearDesignation] =
    readAsInt(s).flatMap { z =>
      if z > 0 then NucleusNumber(PositiveInt.unsafe(z)).asRight
      else if z === 0 then OutsideNucleus.asRight
      else s"Negative value parsed for nucleus number: $z".asLeft
    }

  given showForNuclearDesignation: Show[NuclearDesignation] = Show.show {
    case OutsideNucleus    => "0"
    case nn: NucleusNumber => nn.show
  }
end NuclearDesignation
