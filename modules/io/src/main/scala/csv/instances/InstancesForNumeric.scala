package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import cats.syntax.all.*
import fs2.data.csv.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.{
  NonnegativeInt,
  NonnegativeReal,
  readAsDouble,
  readAsInt
}

trait InstancesForNumeric:
  given cellDecoderForNonnegativeInt: CellDecoder[Int :| Not[Negative]] =
    liftToCellDecoder(readAsInt.map(_.flatMap(NonnegativeInt.either)))

  given (SimpleShow[Int :| Not[Negative]]) => CellEncoder[Int :| Not[Negative]] =
    CellEncoder.fromSimpleShow[Int :| Not[Negative]]

  given cellDecoderForNonnegativeReal: CellDecoder[Double :| Not[Negative]] =
    liftToCellDecoder(readAsDouble.map(_.flatMap(NonnegativeReal.either)))

  given (enc: CellEncoder[Double]) => CellEncoder[Double :| Not[Negative]] =
    enc.contramap: (x: Double :| Not[Negative]) =>
      (x: Double)
end InstancesForNumeric
