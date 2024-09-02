package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import scala.util.NotGiven
import cats.syntax.all.*
import fs2.data.csv.*

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.geometry.Centroid.asPoint
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.instances.encoding.given

trait InstancesForGeometry:
  /* Coordinate decoders */
  given cellDecoderForZCoordinate[A](using
      dec: CellDecoder[A]
  ): CellDecoder[ZCoordinate[A]] = dec.map(ZCoordinate.apply)

  given cellDecoderForYCoordinate[A](using
      dec: CellDecoder[A]
  ): CellDecoder[YCoordinate[A]] = dec.map(YCoordinate.apply)

  given cellDecoderForXCoordinate[A](using
      dec: CellDecoder[A]
  ): CellDecoder[XCoordinate[A]] = dec.map(XCoordinate.apply)

  /** Use the contravariant nature of encoding to build an encoder for a
    * coordinate.
    *
    * Simply encode the coordinate the same way as its raw, unwrapped,
    * underlying value would be encoded in CSV.
    *
    * @tparam A
    *   The wrapped/underlying coordinate value type
    * @tparam C
    *   The coordinate (sub)type constructor
    * @param enc
    *   The [[fs2.data.csv.CellEncoder]] instance for the raw, underlying value
    *   which is wrapped as a coordinate
    */
  given cellEncoderForCoordinate[A, C[A] <: Coordinate[A]: [C[A]] =>> NotGiven[
    C[A] =:= Coordinate[A]
  ]](using enc: CellEncoder[A]): CellEncoder[C[A]] =
    enc.contramap(_.value)

  given csvRowEncoderForCentroid[C](using
      CellEncoder[C]
  ): CsvRowEncoder[Centroid[C], String] = new:
    override def apply(elem: Centroid[C]): RowF[Some, String] =
      val pt = elem.asPoint
      val z = ColumnNames.zCenterColumnName[C].write(pt.z)
      val y = ColumnNames.yCenterColumnName[C].write(pt.y)
      val x = ColumnNames.xCenterColumnName[C].write(pt.x)
      z |+| y |+| x
