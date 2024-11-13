package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import scala.util.NotGiven
import at.ac.oeaw.imba.gerlich.gerlib.geometry.Coordinate

/** JSON-related typeclass instances for geometry-related data types */
trait JsonInstancesForGeometry:
  def getPlainJsonValueWriter[A, C <: Coordinate[
    A
  ], O <: ujson.Value: [C] =>> NotGiven[C =:= Coordinate[A]]](using
      writeRaw: JsonValueWriter[A, O]
  ): JsonValueWriter[C, O] = new:
    override def apply(c: C): O = writeRaw(c.value)

end JsonInstancesForGeometry
