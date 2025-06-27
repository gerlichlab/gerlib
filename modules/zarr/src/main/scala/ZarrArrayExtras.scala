package at.ac.oeaw.imba.gerlich.gerlib.zarr

import scala.util.Try
import cats.syntax.all.*

import com.bc.zarr.ZarrArray
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarr.IndexMapping
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrBlockSize
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrStandardCoordinate

/** Helpers for working with [[com.bc.zarr.ZarrArray]] */
object ZarrArrayExtras:
  
  // TODO: guard against atypical dimension order.
  private def getXY(za: ZarrArray): Either[String, Dimensions2D] = 
    za.getShape.toList match {
      case _ :: _ :: _ :: y :: x :: Nil => new Dimensions2D(x = x, y = y).asRight
      case dims => s"${dims.length}-D array, not 5-D".asLeft
    }

  def readFirstFullSize2DFrom5D(za: ZarrArray): Either[String, DataRead2D] = for
    dims <- getXY(za)
    data <- Try{ 
      JzarrTools.readFrom(za, Array(1, 1, 1, dims.y, dims.x), Array(0, 0, 0, 0, 0)) 
    }.toEither.leftMap(_.getMessage)
  yield DataRead2D(data, dims)

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
  
  final class Dimensions2D(val x: Int, val y: Int):
    require(x >= 0, s"x dimension must be nonnegative, not $x")
    require(y >= 0, s"y dimension must be nonnegative, not $y")

  final class DataRead2D(values: Array[Int], dimensions: Dimensions2D):
    require(
      values.length === dimensions.x * dimensions.y, 
      s"${dimensions.x} * ${dimensions.y} = ${dimensions.x * dimensions.y}, not ${values.length}"
    )
end ZarrArrayExtras
