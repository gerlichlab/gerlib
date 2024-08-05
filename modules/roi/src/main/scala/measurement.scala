package at.ac.oeaw.imba.gerlich.gerlib.roi

import cats.Order
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

package object measurement:
  /** Area of a region of interest */
  opaque type Area = PositiveReal

  object Area:
    extension (a: Area) def toDouble: Double = a

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
    extension (i: MeanIntensity) def toDouble: Double = i

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
