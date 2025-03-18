package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.syntax.all.*
import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.io.csv.*
import at.ac.oeaw.imba.gerlich.gerlib.testing.instances.catsScalacheck.given

/** Testing-related typeclass instances for CSV-related data types */
trait CsvInstances:
  /** Simply generate a column name, and type the wrapper appropriately. */
  given [A] => (arbStr: Arbitrary[String]) => Arbitrary[ColumnNameLike[A]] =
    arbStr.map(ColumnName.apply[A])
end CsvInstances
