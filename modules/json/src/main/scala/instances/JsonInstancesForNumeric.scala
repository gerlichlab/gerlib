package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Typeclass instances related to JSON for numeric data types */
trait JsonInstancesForNumeric:
  given JsonValueWriter[NonnegativeInt, ujson.Num] = JsonValueWriter(identity)

  given JsonValueWriter[NonnegativeReal, ujson.Num] = JsonValueWriter(identity)

  given JsonValueWriter[Boolean, ujson.Bool] =
    JsonValueWriter.instance(ujson.Bool.apply)

  given JsonValueWriter[Double, ujson.Num] =
    JsonValueWriter.instance(ujson.Num.apply)

  given JsonValueWriter[Int, ujson.Num] =
    JsonValueWriter.instance(z => ujson.Num.apply(z.toDouble))
end JsonInstancesForNumeric
