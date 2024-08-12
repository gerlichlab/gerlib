package at.ac.oeaw.imba.gerlich.gerlib.roi

import cats.Order
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

package object measurement:
  /** Area of a region of interest */
  opaque type Area = PositiveReal

  /** Helpers for working with geometric area */
  object Area:
    extension (a: Area) private[gerlib] def toDouble: Double = a

    /** Semantically designate the raw numeric value as an area. */
    def apply(a: PositiveReal): Area = a: Area

    /** Attempt to semantically designate the raw numeric value as an area. */
    def fromDouble(x: Double): Either[String, Area] =
      PositiveReal.either(x).map(apply)

    /** Use normal numeric ordering for ROI area values. */
    given orderForArea(using ordPosNum: Order[PositiveReal]): Order[Area] =
      ordPosNum.contramap(identity)

    /** Attempt to read the given text as a ROI area value. */
    def parse(s: String): Either[String, Area] = PositiveReal.parse(s)
  end Area

  /** Mean values in a region of interest */
  opaque type MeanIntensity = NonnegativeReal

  /** Helpers for working with ROI mean intensity measurement */
  object MeanIntensity:
    extension (i: MeanIntensity) private[gerlib] def toDouble: Double = i

    /** Use normal numeric ordering for ROI mean intensity values. */
    given orderForMeanIntensity(using
        ordNonNegNum: Order[NonnegativeReal]
    ): Order[MeanIntensity] =
      ordNonNegNum.contramap(identity)

    /** Attempt to read the given text as a ROI mean intensity value. */
    def parse(s: String): Either[String, MeanIntensity] =
      NonnegativeReal.parse(s)
  end MeanIntensity
end measurement
