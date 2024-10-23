package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package instances

import cats.Semigroup
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.io.csv.NamedRow
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for [[fs2.data.csv.CsvRowEncoder]] */
trait InstancesForEncoding:
  given Semigroup[NamedRow] with
    override def combine(a: NamedRow, b: NamedRow): NamedRow =
      val aNames = a.headers.extractValue
      val bNames = b.headers.extractValue
      val repeats = aNames.toNes & bNames.toNes
      if repeats.nonEmpty then
        throw new IllegalArgumentException(
          s"${repeats.size} names repeated between row parts to combine: ${repeats.mkString_("; ")}"
        )
      NamedRow(Some(aNames ::: bNames), a.values ::: b.values)
end InstancesForEncoding
