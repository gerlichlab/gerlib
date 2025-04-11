package at.ac.oeaw.imba.gerlich.gerlib.roi

import cats.Order
import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.{Negative, Positive}

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

package object measurement:
  /** Area of a region of interest */
  opaque type Area = Double :| Positive

  /** Helpers for working with geometric area */
  object Area:
    extension (a: Area) private[gerlib] def toDouble: Double = a

    /** Semantically designate the raw numeric value as an area. */
    def apply(a: Double :| Positive): Area = a: Area

    /** Attempt to semantically designate the raw numeric value as an area. */
    def fromDouble(x: Double): Either[String, Area] =
      PositiveReal.either(x).map(apply)

    /** Use normal numeric ordering for ROI area values. */
    given (ordPosNum: Order[Double :| Positive]) => Order[Area] =
      ordPosNum.contramap(identity)

    /** Attempt to read the given text as a ROI area value. */
    def parse(s: String): Either[String, Area] = readAsDouble(s).flatMap(PositiveReal.either)
  end Area

  /** Mean values in a region of interest */
  opaque type MeanIntensity = Double :| Not[Negative]

  /** Helpers for working with ROI mean intensity measurement */
  object MeanIntensity:
    extension (i: MeanIntensity) private[gerlib] def toDouble: Double = i

    /** Semantically designate the raw numeric value as an area. */
    def apply(i: Double :| Not[Negative]): MeanIntensity = i: MeanIntensity

    /** Attempt to semantically designate the raw numeric value as an area. */
    def fromDouble(x: Double): Either[String, MeanIntensity] =
      NonnegativeReal.either(x).map(apply)

    /** Use normal numeric ordering for ROI mean intensity values. */
    given (ordNonNegNum: Order[Double :| Not[Negative]]) => Order[MeanIntensity] =
      ordNonNegNum.contramap(identity)

    /** Attempt to read the given text as a ROI mean intensity value. */
    def parse(s: String): Either[String, MeanIntensity] =
      readAsDouble(s).flatMap(NonnegativeReal.either)
  end MeanIntensity
end measurement
