package at.ac.oeaw.imba.gerlich.gerlib.numeric
package instances

import cats.*
import io.github.iltotore.iron.cats.given

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.json.JsonValueWriter

trait NonnegativeIntInstances:
  given IntLike[NonnegativeInt] with
    override def asInt = identity
  given JsonValueWriter[NonnegativeInt, ujson.Num] with
    override def apply(n: NonnegativeInt): ujson.Num = ujson.Num(n)
  given Order[NonnegativeInt] = summon[Order[NonnegativeInt]]
  given Show[NonnegativeInt] = summon[Show[NonnegativeInt]]
  given SimpleShow[NonnegativeInt] = SimpleShow.fromShow
end NonnegativeIntInstances
