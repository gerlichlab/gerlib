package at.ac.oeaw.imba.gerlich.gerlib.io

import scala.util.NotGiven

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.effect.IO
import fs2.*
import fs2.data.*
import fs2.data.csv.*
import fs2.data.text.CharLikeChunks
import fs2.io.file.{Files, Path as FS2Path}

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.syntax.option.* // for .extractValue

package object csv:
  /** Wrap the given parse attempt function in a [[fs2.data.csv.CellEncoder]],
    * then map over it with the given builder.
    *
    * @tparam A
    *   The wrapped/underlying coordinate value type
    * @tparam C
    *   The coordinate (sub)type constructor
    * @param parseA
    *   How to try to parse a raw `A` value from text
    * @param liftA
    *   How to wrap the raw `A` value as a coordinate value
    * @return
    *   A decoder for a single field of a CSV
    */
  def getCellDecoderForCoordinate[A, C[A] <: Coordinate[A]: [C[A]] =>> NotGiven[
    C[A] =:= Coordinate[A]
  ]](
      parse: String => Either[String, C[A]]
  ): CellDecoder[C[A]] = liftToCellDecoder(parse)

  /** Combine two decoders and a builder to get a decoder for the target output
    * type.
    */
  def getCsvRowDecoderForProduct2[I1, I2, O, Head](build: (I1, I2) => O)(using
      decI1: CsvRowDecoder[I1, Head],
      decI2: CsvRowDecoder[I2, Head]
  ): CsvRowDecoder[O, Head] = new:
    override def apply(row: RowF[Some, Head]): DecoderResult[O] =
      val i1Nel = decI1(row).toValidatedNel
      val i2Nel = decI2(row).toValidatedNel
      (i1Nel, i2Nel)
        .mapN(build)
        .toEither
        .leftMap(_.map(_.getMessage))
        .leftMap { msgs =>
          DecoderError(s"Error(s) decoding row ($row): ${msgs.mkString_("; ")}")
        }

  /** Simply concatenate the values and headers for the two members of a product
    * type.
    *
    * @tparam I1
    *   The type of the first "field" of the product type for which to build an
    *   encoder
    * @tparam I2
    *   The type of the second "field" of the product type for which to build an
    *   encoder
    * @param getI1
    *   How to get the first "field" value from a value of the given product
    *   type `O`
    * @param getI2
    *   How to get the second "field" value from a value of the given product
    *   type `O`
    * @param encI1
    *   The encoder for values of the type of the product's first "field"
    * @param encI2
    *   The encoder for values of the type of the product's second "field"
    * @return
    *   An encoder for single field/cell for value of type `O` when writing CSV
    */
  def getCsvRowEncoderForProduct2[O, I1, I2, Head](
      getI1: O => I1,
      getI2: O => I2
  )(using
      encI1: CsvRowEncoder[I1, Head],
      encI2: CsvRowEncoder[I2, Head]
  ): CsvRowEncoder[O, Head] =
    new:
      override def apply(elem: O): RowF[Some, Head] =
        val part1 = encI1(getI1(elem))
        val part2 = encI2(getI2(elem))
        val vs = part1.values ::: part2.values
        val hs = Some(part1.headers.extractValue ::: part2.headers.extractValue)
        RowF(vs, hs)

  /** Turn the implicit/given cell decoder into a row decoder by parsing a row's
    * value at the given field/key.
    */
  def getCsvRowDecoderForSingleton[T: CellDecoder](
      key: ColumnNameLike[T]
  ): CsvRowDecoder[T, String] = new:
    override def apply(row: RowF[Some, String]): DecoderResult[T] =
      row.as[T](key.value)

  /** Turn the implicit/given cell encoder into a row encoder by giving the
    * field the assigned name (`key`).
    */
  def getCsvRowEncoderForSingleton[T: CellEncoder](
      key: ColumnNameLike[T]
  ): CsvRowEncoder[T, String] =
    getCsvRowEncoderForSingleton(key.value)

  /** Turn the implicit/given cell encoder into a row encoder by giving the
    * field the assigned name (`key`).
    */
  def getCsvRowEncoderForSingleton[T](key: String)(using
      enc: CellEncoder[T]
  ): CsvRowEncoder[T, String] = new:
    override def apply(elem: T): RowF[Some, String] =
      RowF(NonEmptyList.one(enc(elem)), Some(NonEmptyList.one(key)))

  /** Wrap the given parsing function as a CSV cell/field encoder, turning the
    * message into an error in fail case.
    */
  def liftToCellDecoder[A](parse: String => Either[String, A]): CellDecoder[A] =
    CellDecoder.instance { (s: String) =>
      parse(s).leftMap(msg => DecoderError(msg))
    }

  /** Attempt to read the given file as a list of case class instances. */
  def readCsvToCaseClasses[A](
      path: FS2Path
  )(using CsvRowDecoder[A, String], CharLikeChunks[IO, Byte]): IO[List[A]] =
    Files[IO]
      .readAll(path)
      .through(decodeUsingHeaders[A]())
      .compile
      .toList

  /** Attempt to read the given file as a list of case class instances. */
  def readCsvToCaseClasses[A](
      path: os.Path
  )(using CsvRowDecoder[A, String], CharLikeChunks[IO, Byte]): IO[List[A]] =
    readCsvToCaseClasses(FS2Path.fromNioPath(path.toNIO))

  /** Get a writer for case class instances of the given type, writing to the
    * given file.
    */
  def writeCaseClassesToCsv[A](path: os.Path)(using
      CsvRowEncoder[A, String]
  ): Pipe[IO, A, Nothing] =
    writeCaseClassesToCsv(FS2Path.fromNioPath(path.toNIO))

  /** Get a writer for case class instances of the given type, writing to the
    * given file.
    */
  def writeCaseClassesToCsv[A](path: FS2Path)(using
      CsvRowEncoder[A, String]
  ): Pipe[IO, A, Nothing] =
    // Adapted from typelevel examples: https://typelevel.org/toolkit/examples.html
    _.through(encodeUsingFirstHeaders(fullRows = true))
      .through(fs2.text.utf8.encode)
      .through(Files[IO].writeAll(path))
