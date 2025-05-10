package at.ac.oeaw.imba.gerlich.gerlib.numeric.instances

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

/** Simple typeclass instances for nonnegative integers */
trait NonnegativeIntInstances:
  given SimpleShow[Int :| Not[Negative]] =
    import io.github.iltotore.iron.cats.given_Show_:|
    SimpleShow.fromShow
end NonnegativeIntInstances
