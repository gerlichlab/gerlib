package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import cats.syntax.all.*
import io.github.iltotore.iron.cats.given

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait NonnegativeRealInstances:
  given orderForNonnegativeReal: Order[NonnegativeReal] =
    summon[Order[NonnegativeReal]]
  given (ev: Show[Double]) => Show[NonnegativeReal] =
    ev.contramap(identity)
  given (Show[Double]) => SimpleShow[NonnegativeReal] =
    SimpleShow.fromShow
  given Subtraction[NonnegativeReal, Double, Double]:
    def minus(minuend: NonnegativeReal)(subtrahend: Double): Double =
      minuend - subtrahend
