package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import scala.util.NotGiven
import squants.space.Length
import ujson.IncompleteParseException
import upickle.default.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.{Coordinate, Distance}

/** JSON-related typeclass instances for geometry-related data types */
trait JsonInstancesForGeometry:
  def getPlainJsonValueWriter[A, C <: Coordinate[
    A
  ], O <: ujson.Value: [C] =>> NotGiven[C =:= Coordinate[A]]](using
      writeRaw: JsonValueWriter[A, O]
  ): JsonValueWriter[C, O] = new:
    override def apply(c: C): O = writeRaw(c.value)

  given (ReadWriter[String]) => ReadWriter[Distance] = readwriter[String].bimap(
    _.toString,
    s =>
      Length(s).fold(
        throw _,
        l =>
          Distance
            .either(l)
            .fold(
              msg => throw new IncompleteParseException(s"(parsing $l): $msg"),
              identity
            )
      )
  )
end JsonInstancesForGeometry
