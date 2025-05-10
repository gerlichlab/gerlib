package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

import scala.util.{NotGiven, Try}
import cats.data.ValidatedNel
import cats.syntax.all.*
import squants.space.Length
import ujson.IncompleteParseException
import upickle.default.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.{Coordinate, Distance, DistanceLike}
import at.ac.oeaw.imba.gerlich.gerlib.geometry.EuclideanDistance

/** JSON-related typeclass instances for geometry-related data types */
trait JsonInstancesForGeometry:
  def getPlainJsonValueWriter[A, C <: Coordinate[
    A
  ], O <: ujson.Value: [C] =>> NotGiven[C =:= Coordinate[A]]](using
      writeRaw: JsonValueWriter[A, O]
  ): JsonValueWriter[C, O] = new:
    override def apply(c: C): O = writeRaw(c.value)

  given ReadWriter[String] => ReadWriter[Length] = readwriter[String].bimap(
    _.toString,
    s => Length(s).fold(throw _, identity)
  )

  given (ReadWriter[Length]) => ReadWriter[Distance] = readwriter[Length].bimap(
    identity,
    l => lengthToDistance(l).fold(throw _, identity)
  )

  given ReadWriter[Distance] => ReadWriter[DistanceLike] =
    val valueKey = "value"
    val typeKey = "metricType"
    val euclideanSpecifier = "Euclidean"
    readwriter[Map[String, ujson.Value]].bimap(
      (_: DistanceLike) match {
      case eucl: EuclideanDistance =>
        Map(
          valueKey -> write(eucl.getDistanceValue),
          typeKey -> euclideanSpecifier
        )
      },
      data =>
        val valueNel = data
          .get(valueKey)
          .toRight(s"Missing value key ($valueKey) for distance")
          .flatMap(s =>
            Try(read[Distance](s)).toEither
              .leftMap(e => s"Cannot decode distance value ($s): ${e.getMessage}")
          )
          .toValidatedNel
        val builderNel: ValidatedNel[String, Distance => DistanceLike] = data
          .get(typeKey)
          .toRight(s"Missing type key ($typeKey) for distance")
          .flatMap(s =>
            Try(read[String](s)).toEither
              .leftMap(_.getMessage)
              .flatMap {
                case `euclideanSpecifier` => EuclideanDistance.apply.asRight
                case unknownSpecifier =>
                  s"Unrecognized distance metric specifier ($unknownSpecifier)".asLeft
              }
          )
          .toValidatedNel
        (valueNel, builderNel)
          .mapN { (dist, build) => build(dist) }
          .fold(
            messages => throw IncompleteParseException(s"Error(s): ${messages.mkString_("; ")}"),
            identity
          )
    )

  private def lengthToDistance(l: Length): Either[IncompleteParseException, Distance] =
    Distance
      .either(l)
      .leftMap(msg => new IncompleteParseException(s"(parsing $l): $msg"))

end JsonInstancesForGeometry
