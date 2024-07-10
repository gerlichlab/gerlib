package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

trait InstancesForNumeric:
  given CellDecoder[NonnegativeReal] = liftToCellDecoder(NonnegativeReal.parse)

  given cellEncoderForNonnegativeReal(using
      enc: CellEncoder[Double]
  ): CellEncoder[NonnegativeReal] =
    enc.contramap { (x: NonnegativeReal) => (x: Double) }
