package at.ac.oeaw.imba.gerlich.gerlib.io
package csv

import cats.data.NonEmptyList
import cats.syntax.all.*

/** A delimiter between values within a CSV cell holding a collection of values of a particular type
  *
  * @tparam A
  *   The type of values stored in a collection in a single CSV cell
  * @param get
  *   The underlying text delimiter to use between individual values in a text representation of the
  *   whole CSV cell
  */
final case class IntraCellDelimiter[A](private val get: String)

/** Helpers for working with intra-CSV-cell delimiter values */
object IntraCellDelimiter:
  /** Syntax convenience for working with intra-cell delimiters */
  object syntax:
    extension (sep: String)
      /** Use the syntax-enriched string a delimiter for within a CSV cell. */
      def toIntraCellDelimiter[A]: IntraCellDelimiter[A] = IntraCellDelimiter(sep)

    /** Add some syntax helpers to a delimiter value. */
    extension [A](sep: IntraCellDelimiter[A])
      def join(ss: Iterable[A])(str: A => String): String = ss.map(str).mkString(sep.get)
      def join(ss: NonEmptyList[A])(str: A => String): String = ss.map(str).mkString_(sep.get)
end IntraCellDelimiter
