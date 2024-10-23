package at.ac.oeaw.imba.gerlich.gerlib

import cats.data.NonEmptySet
import cats.syntax.all.*

/** Tools for working with collections */
object collections:
  extension [A](bag: Set[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)

  extension [A](bag: NonEmptySet[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)
end collections
