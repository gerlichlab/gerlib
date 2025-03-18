package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import io.github.iltotore.iron.cats.given
import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait PositiveIntInstances:
  given IntLike[PositiveInt]:
    override def asInt = identity
  given Order[PositiveInt] = summon[Order[PositiveInt]]
  given Show[PositiveInt] = summon[Show[PositiveInt]]
  given SimpleShow[PositiveInt] = SimpleShow.fromShow
