package at.ac.oeaw.imba.gerlich.gerlib.zarr

import scala.util.Try
import cats.syntax.all.*

import com.bc.zarr.ZarrArray
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarr.IndexMapping
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrBlockSize
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrStandardCoordinate

/** Helpers for working with [[com.bc.zarr.ZarrArray]] */
object ZarrArrayExtras:
  extension (za: ZarrArray)
    def read(indexMapping: IndexMapping)(
        origin: OmeZarrStandardCoordinate,
        size: OmeZarrBlockSize
    ): Either[String, Array[Int]] =
      val originIndex = indexMapping.buildOriginIndex(origin)
      val sizeIndex = indexMapping.buildSizeIndex(size)
      Try:
        JzarrTools.readFrom(za, sizeIndex, originIndex)
      .toEither
        .leftMap(e =>
          s"For index mapping $indexMapping and starting from $origin, failed to read block of size $size: ${e.getMessage}"
        )
end ZarrArrayExtras
