package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import scala.util.Try
import cats.Alternative
import cats.syntax.all.*
import ujson.IncompleteParseException
import upickle.default.{Reader, reader}
import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2

/** JSON-related typeclass instances collections */
trait JsonInstancesForCollections:
  /** Read a list of at least 2 elements of the target type from JSON. */
  given readerForAtLeast2List[A: Reader]: Reader[AtLeast2[List, A]] =
    reader[ujson.Value].map(json =>
      json.arrOpt match
      case None => throw IncompleteParseException(s"Cannot read value as array-like: $json")
      case Some(vs) =>
        Alternative[List].separate(
          vs.toList.map { v =>
            Try:
              v.transform[A](summon[Reader[A]])
            .toEither
            .leftMap(e => s"Cannot parse value ($v): ${e.getMessage}")
          }
        ) match
        case (Nil, parsedValues) =>
          AtLeast2
            .either(parsedValues)
            .fold(
              msg => throw IncompleteParseException(msg),
              identity
            )
        case (errorMessages, _) =>
          throw IncompleteParseException(
            s"${errorMessages.length} error(s) parsing values: ${errorMessages.mkString("; ")}"
          )
    )
end JsonInstancesForCollections
