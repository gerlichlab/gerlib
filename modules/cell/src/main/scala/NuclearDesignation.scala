package at.ac.oeaw.imba.gerlich.gerlib.cell

import cats.*
import cats.instances.int.*
import cats.derived.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.positiveInt.given

/** Designation of whether something's in a cell nucleus or not */
sealed trait NuclearDesignation

/** Something that's not in a cell nucleus */
case object OutsideNucleus extends NuclearDesignation

/** A natural number with which to label / identify a nucleus in a field of view
  */
final case class NucleusNumber(get: PositiveInt) extends NuclearDesignation derives Order

object NucleusNumber:
  /** Show the nucleus number by the underlying, wrapped numeric value. */
  given SimpleShow[NucleusNumber] = SimpleShow.instance(_.get.show)

  /** Try to read the given string as a nucleus number. */
  def parse(s: String): Either[String, NucleusNumber] =
    readAsInt(s)
      .flatMap(PositiveInt.either)
      .bimap(
        msg => s"Cannot parse value ($s) as nucleus number: $msg",
        NucleusNumber.apply
      )
end NucleusNumber

/** Helpers for working with nuclei number labels */
object NuclearDesignation:
  /** Order nuclear designation with non-nuclear first, then by number. */
  def orderWithNonNuclearFirst: Order[NuclearDesignation] = new:
    override def compare(a: NuclearDesignation, b: NuclearDesignation): Int =
      (a, b) match
      case (OutsideNucleus, OutsideNucleus)       => 0
      case (OutsideNucleus, NucleusNumber(_))     => -1
      case (NucleusNumber(_), OutsideNucleus)     => 1
      case (NucleusNumber(n1), NucleusNumber(n2)) => n1 - n2

  /** Attempt to read the given text as a nucleus number. */
  def parse(s: String): Either[String, NuclearDesignation] =
    readAsInt(s).flatMap { z =>
      if z > 0 then NucleusNumber(PositiveInt.unsafe(z)).asRight
      else if z === 0 then OutsideNucleus.asRight
      else s"Negative value parsed for nucleus number: $z".asLeft
    }

  /** Represent the extranuclear designation as 0, and intranuclear by the wrapped number. */
  given SimpleShow[NuclearDesignation]:
    override def show_(nd: NuclearDesignation): String = nd match
    case OutsideNucleus    => "0"
    case nn: NucleusNumber => nn.show_
end NuclearDesignation
