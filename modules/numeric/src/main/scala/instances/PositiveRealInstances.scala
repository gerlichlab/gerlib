package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import cats.syntax.all.*
import io.github.iltotore.iron.cats.given
import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait PositiveRealInstances:
  given Order[PositiveReal] = summon[Order[PositiveReal]]
  given (ev: Show[Double]) => Show[PositiveReal] =
    ev.contramap(identity)
  given (Show[Double]) => SimpleShow[PositiveReal] =
    SimpleShow.fromShow
  given Subtraction[PositiveReal, Double, Double]:
    def minus(minuend: PositiveReal)(subtrahend: Double): Double =
      minuend - subtrahend
