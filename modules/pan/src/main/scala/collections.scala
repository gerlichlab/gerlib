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

  /** Partition a collection of objects into {@code n} distinct parts (subsets).
    *
    * WARNING: not very performant, so use only with small input collections.
    *
    * @tparam X
    *   The type of object in the collection to partition
    * @param n
    *   The number of (disjoint) subsets with which to cover the input set
    * @param xs
    *   The input set to partition
    * @return
    *   A collection in which each element is a disjoint covering (i.e., a
    *   partition) of the given input collection
    */
  def partition[X](n: Int, xs: Set[X]): List[List[Set[X]]] = {
    require(
      n > 0,
      s"Desired number of subsets must be strictly postitive, not $n"
    )
    require(
      n <= xs.size,
      s"Desired number of subsets exceeds number of elements: $n > ${xs.size}"
    )
    powerset(xs)
      .filter(_.nonEmpty)
      .combinations(n)
      .foldRight(List.empty[List[Set[X]]]) { (subs, acc) =>
        val part = subs.map(_.toSet)
        if part.map(_.size).sum === xs.size && part.combineAll === xs then
          (part :: acc)
        else acc
      }
  }

  /** Give the powerset of the given set, including the emtpy set and the set
    * itself.
    */
  private def powerset[X](xs: Set[X]): List[Seq[X]] =
    List.range(0, xs.size + 1).flatMap(xs.toSeq.combinations(_).toList)
end collections
