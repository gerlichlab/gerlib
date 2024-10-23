package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

trait InstancesForNumeric:
  given CellDecoder[NonnegativeInt] = liftToCellDecoder(NonnegativeInt.parse)

  given cellEncoderForNonnegativeInt(using
      SimpleShow[NonnegativeInt]
  ): CellEncoder[NonnegativeInt] = CellEncoder.fromSimpleShow[NonnegativeInt]

  given CellDecoder[NonnegativeReal] = liftToCellDecoder(NonnegativeReal.parse)

  given cellEncoderForNonnegativeReal(using
      enc: CellEncoder[Double]
  ): CellEncoder[NonnegativeReal] =
    enc.contramap: (x: NonnegativeReal) =>
      (x: Double)
