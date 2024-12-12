package at.ac.oeaw.imba.gerlich.gerlib
package syntax

import cats.Order
import cats.syntax.all.*

/** Syntax enrichment for tuples */
trait SyntaxForTuple2:
  extension [A](t: (A, A))
    /** Flip the given tuple such that the first element is no greater than the second, according to
      * the given key function and the ordering for the key function's codomain.
      */
    def flipBy[K: Order](key: A => K): Tuple2[A, A] =
      if key(t._2) < key(t._1) then t.swap else t

    /** Alias for [[flipBy]] */
    def swapBy[K: Order](key: A => K): Tuple2[A, A] = flipBy(key)
end SyntaxForTuple2
