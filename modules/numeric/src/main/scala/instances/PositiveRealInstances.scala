package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.Show
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.numeric.Positive

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait PositiveRealInstances:
  given (Show[Double]) => SimpleShow[Double :| Positive] =
    import io.github.iltotore.iron.cats.given_Show_:|
    SimpleShow.fromShow
  given Subtraction[Double :| Positive, Double, Double]:
    def minus(minuend: Double :| Positive)(subtrahend: Double): Double =
      minuend - subtrahend
