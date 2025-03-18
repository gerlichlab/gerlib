package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import scala.util.NotGiven
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.geometry.Centroid.asPoint
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.instances.encoding.given

/** Typeclass instances for geometry-related data types */
trait InstancesForGeometry:
  /* Coordinate decoders */
  given [A] => (dec: CellDecoder[A]) => CellDecoder[ZCoordinate[A]] = dec.map(ZCoordinate.apply)

  given [A] => (dec: CellDecoder[A]) => CellDecoder[YCoordinate[A]] = dec.map(YCoordinate.apply)

  given [A] => (dec: CellDecoder[A]) => CellDecoder[XCoordinate[A]] = dec.map(XCoordinate.apply)

  /** Use the contravariant nature of encoding to build an encoder for a coordinate.
    *
    * Simply encode the coordinate the same way as its raw, unwrapped, underlying value would be
    * encoded in CSV.
    *
    * @tparam A
    *   The wrapped/underlying coordinate value type
    * @tparam C
    *   The coordinate (sub)type constructor
    * @param enc
    *   The [[fs2.data.csv.CellEncoder]] instance for the raw, underlying value which is wrapped as
    *   a coordinate
    */
  given [A, C[A] <: Coordinate[A]: [C[A]] =>> NotGiven[
    C[A] =:= Coordinate[A]
  ]] => (enc: CellEncoder[A]) => CellEncoder[C[A]] =
    enc.contramap(_.value)

  given [C] => (
      CellDecoder[XCoordinate[C]],
      CellDecoder[YCoordinate[C]],
      CellDecoder[ZCoordinate[C]]
  ) => CsvRowDecoder[Centroid[C], String] = new:
    override def apply(row: RowF[Some, String]): DecoderResult[Centroid[C]] =
      val xNel = ColumnNames.xCenterColumnName[C].from(row)
      val yNel = ColumnNames.yCenterColumnName[C].from(row)
      val zNel = ColumnNames.zCenterColumnName[C].from(row)
      (xNel, yNel, zNel)
        .mapN: (x, y, z) =>
          Centroid.fromPoint(Point3D(x, y, z))
        .toEither
        .leftMap { messages =>
          DecoderError(
            s"Cannot decode imaging context because of ${messages.length} error(s): ${messages.mkString_("; ")}"
          )
        }

  given [C: CellEncoder] => CsvRowEncoder[Centroid[C], String] = new:
    override def apply(elem: Centroid[C]): RowF[Some, String] =
      val pt = elem.asPoint
      val z = ColumnNames.zCenterColumnName[C].write(pt.z)
      val y = ColumnNames.yCenterColumnName[C].write(pt.y)
      val x = ColumnNames.xCenterColumnName[C].write(pt.x)
      z |+| y |+| x
