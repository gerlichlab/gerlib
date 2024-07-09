package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import cats.syntax.all.*
import io.github.iltotore.iron.cats.given
import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait NonnegativeRealInstances:
  given orderForNonnegativeReal: Order[NonnegativeReal] =
    summon[Order[NonnegativeReal]]
  given showForNonnegativeReal(using ev: Show[Double]): Show[NonnegativeReal] =
    ev.contramap(identity)
  given simpleShowForNonnegativeReal(using
      Show[Double]
  ): SimpleShow[NonnegativeReal] =
    SimpleShow.fromShow
  given Subtraction[NonnegativeReal, Double, Double] with
    def minus(minuend: NonnegativeReal)(subtrahend: Double): Double =
      minuend - subtrahend
