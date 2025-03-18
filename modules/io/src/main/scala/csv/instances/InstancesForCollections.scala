package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import fs2.data.csv.*
import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2
import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2.syntax.*
import at.ac.oeaw.imba.gerlich.gerlib.io.csv.IntraCellDelimiter.syntax.*

/** CSV-related typeclass instances for collections */
trait InstancesForCollections:
  private def cellEncoderForIterable[C[*] <: Iterable[*], X](
      encX: CellEncoder[X],
      sep: IntraCellDelimiter[X]
  ): CellEncoder[C[X]] = new:
    override def apply(cell: C[X]): String = sep.join(cell)(encX.apply)

  /** Create a cell encoder by using the element encoder and joining on the given delimiter.
    *
    * @tparam C
    *   The type of container for the collection to encode
    * @tparam X
    *   The type of elements in the container for the collection to encode
    * @param enc
    *   Encoder for the collection's element type
    * @param sep
    *   Delimiter between encoded elements of the underlying collection
    * @return
    *   A builder of the text representation of a collection of {@code X} values in a container
    *   {@code C}
    */
  given [C[*] <: Iterable[*], X] => (enc: CellEncoder[X], sep: IntraCellDelimiter[X])
    => CellEncoder[AtLeast2[C, X]] =
    val encode = cellEncoderForIterable(enc, sep)
    new:
      override def apply(cell: AtLeast2[C, X]): String = encode(cell.toNativeIterable)
end InstancesForCollections
