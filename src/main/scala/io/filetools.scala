package at.ac.oeaw.imba.gerlich.gerlib.io

import cats.effect.IO
import fs2.*
import fs2.io.file.{ Files, Path as FS2Path }
import fs2.data.*
import fs2.data.csv.*
import fs2.data.text.CharLikeChunks

/** Tools for working with files */
object filetools:
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
end filetools
