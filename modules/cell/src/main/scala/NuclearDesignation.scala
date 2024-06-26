package at.ac.oeaw.imba.gerlich.gerlib.cell

import cats.*
import cats.derived.*
import cats.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Designation of whether something's in a cell nucleus or not */
sealed trait NuclearDesignation:
    /** Represent the nucleus designation as a simple integer.  */
    final def asInt: Int = asNonnegativeInt.asInt
    /** Represent the nucleus designation as a simple nonnegative integer. */
    def asNonnegativeInt: NonnegativeInt

/** Something that's not in a cell nucleus */
case object OutsideNucleus extends NuclearDesignation:
    /** */
    final override def asNonnegativeInt: NonnegativeInt = NonnegativeInt(0)

/** A natural number with which to label / identify a nucleus in a field of view */
final case class NucleusNumber(get: PositiveInt) extends NuclearDesignation derives Order, Show:
    import PositiveInt.*
    /** */
    final override def asNonnegativeInt: NonnegativeInt = get.asNonnegativeInt

/** Helpers for working with nuclei number labels */
object NuclearDesignation:
    /** Attempt to read the given text as a nucleus number. */
    def parse(s: String): Either[String, NuclearDesignation] = readAsInt(s).flatMap{ z => 
        if z > 0 then NucleusNumber(PositiveInt.unsafe(z)).asRight
        else if z === 0 then OutsideNucleus.asRight
        else s"Negative value parsed for nucleus number: $z".asLeft
    }
end NuclearDesignation
