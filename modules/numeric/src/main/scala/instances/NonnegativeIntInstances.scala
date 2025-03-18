package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import io.github.iltotore.iron.cats.given

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait NonnegativeIntInstances:
  given IntLike[NonnegativeInt]:
    override def asInt = identity
  given Order[NonnegativeInt] = summon[Order[NonnegativeInt]]
  given Show[NonnegativeInt] = summon[Show[NonnegativeInt]]
  given SimpleShow[NonnegativeInt] = SimpleShow.fromShow
end NonnegativeIntInstances
