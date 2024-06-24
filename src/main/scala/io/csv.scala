package at.ac.oeaw.imba.gerlich.gerlib.io

import scala.util.NotGiven
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import fs2.*
import fs2.io.file.{ Files, Path as FS2Path }
import fs2.data.*
import fs2.data.csv.*
import fs2.data.text.CharLikeChunks

import at.ac.oeaw.imba.gerlich.gerlib.geometry.*
import at.ac.oeaw.imba.gerlich.gerlib.syntax.option.*

/** Tools for working with files */
object csv:

    def getCellDecoderForCoordinate[A, C[A] <: Coordinate[A] : [C[A]] =>> NotGiven[C[A] =:= Coordinate[A]]](
        parseA: String => Either[String, A], 
        liftA: A => C[A],
    ): CellDecoder[C[A]] = 
        liftToCellDecoder{ parseA.map(_.map(liftA)) }

    given cellEncoderForCoordinate[A : Numeric, C[A] <: Coordinate[A] : [C[A]] =>> NotGiven[C[A] =:= Coordinate[A]]](using enc: CellEncoder[A]): CellEncoder[C[A]] =
        // NB: here the Numeric[A] is needed to prove valid access to the A which the coordinate wraps (it's .get which requires the Numeric.)
        enc.contramap(_.get)

    def liftToCellDecoder[A](parse: String => Either[String, A]): CellDecoder[A] = 
        CellDecoder.instance{ (s: String) => parse(s).leftMap(msg => DecoderError(msg)) }

    /** Combine two decoders and a builder to get a decoder for the target output type. */
    def getCsvRowDecoderForProduct2[I1, I2, O, Head](build: (I1, I2) => O)(using decI1: CsvRowDecoder[I1, Head], decI2: CsvRowDecoder[I2, Head]): CsvRowDecoder[O, Head] = new:
        override def apply(row: RowF[Some, Head]): DecoderResult[O] =
            val i1Nel = decI1(row).toValidatedNel
            val i2Nel = decI2(row).toValidatedNel
            (i1Nel, i2Nel)
                .mapN(build)
                .toEither
                .leftMap(_.map(_.getMessage))
                .leftMap{ msgs => DecoderError(s"Error(s) decoding row ($row): ${msgs.mkString_("; ")}") }

    def getCsvRowEncoderForProduct2[O, I1, I2, Head](getI1: O => I1, getI2: O => I2)(using encI1: CsvRowEncoder[I1, Head], encI2: CsvRowEncoder[I2, Head]): CsvRowEncoder[O, Head] =
        new:
            override def apply(elem: O): RowF[Some, Head] =
                val part1 = encI1(getI1(elem))
                val part2 = encI2(getI2(elem))
                val vs = part1.values ::: part2.values
                val hs = Some(part1.headers.extractValue ::: part2.headers.extractValue)
                RowF(vs, hs)

    def getCsvRowDecoderForSingleton[T](key: String)(using CellDecoder[T]): CsvRowDecoder[T, String] = new:
        override def apply(row: RowF[Some, String]): DecoderResult[T] = row.as[T](key)

    def getCsvRowEncoderForSingleton[T](key: String)(using enc: CellEncoder[T]): CsvRowEncoder[T, String] = new:
        override def apply(elem: T): RowF[Some, String] = RowF(NonEmptyList.one(enc(elem)), Some(NonEmptyList.one(key)))

    def readCsvToCaseClasses[A](path: FS2Path)(using CsvRowDecoder[A, String], CharLikeChunks[IO, Byte]): IO[List[A]] =
        Files[IO].readAll(path)
            .through(decodeUsingHeaders[A]())
            .compile
            .toList

    def readCsvToCaseClasses[A](path: os.Path)(using CsvRowDecoder[A, String], CharLikeChunks[IO, Byte]): IO[List[A]] =
        readCsvToCaseClasses(FS2Path.fromNioPath(path.toNIO))

    def writeCaseClassToCsv[A](path: os.Path)(using CsvRowEncoder[A, String]): Pipe[IO, A, Nothing] =
        writeCaseClassToCsv(FS2Path.fromNioPath(path.toNIO))

    // Adapted from typelevel examples: https://typelevel.org/toolkit/examples.html
    def writeCaseClassToCsv[A](path: FS2Path)(using CsvRowEncoder[A, String]): Pipe[IO, A, Nothing] =
        _.through(encodeUsingFirstHeaders(fullRows = true))
            .through(fs2.text.utf8.encode)
            .through(Files[IO].writeAll(path))
end csv
