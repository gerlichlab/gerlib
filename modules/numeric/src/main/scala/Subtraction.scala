package at.ac.oeaw.imba.gerlich.gerlib.numeric

trait Subtraction[Min, Sub, Result]:
  infix def minus(minuend: Min)(subtrahend: Sub): Result

object Subtraction:
  import scala.math.Numeric.Implicits.infixNumericOps

  given [A: Numeric] => Subtraction[A, A, A]:
    infix def minus(minuend: A)(subtrahend: A): A = minuend - subtrahend
  extension [Min](minuend: Min)
    infix def minus[Sub, Result](subtrahend: Sub)(using
        ev: Subtraction[Min, Sub, Result]
    ): Result =
      ev.minus(minuend)(subtrahend)
