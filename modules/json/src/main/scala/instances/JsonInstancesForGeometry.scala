package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import scala.util.NotGiven
import cats.syntax.all.*
import squants.space.Length
import upickle.default.*
import at.ac.oeaw.imba.gerlich.gerlib.geometry.{Coordinate, Distance}
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

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
      Length(s).toEither
        .flatMap(l => Distance.either(l).leftMap(msg => IllegalRefinement(l, msg)))
        .fold(throw _, identity)
  )
end JsonInstancesForGeometry
