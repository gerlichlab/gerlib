package at.ac.oeaw.imba.gerlich.gerlib

import cats.Functor
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all.*
import io.github.iltotore.iron.{:|, Constraint, refineEither, refineUnsafe}
import io.github.iltotore.iron.constraint.collection.MinLength

/** Tools for working with collections */
object collections:

  /** A collection which must contain at least two elements
    *
    * @tparam C
    *   The container/collection type (constructor)
    * @tparam E
    *   The element type
    */
  type AtLeast2[C[*], E] = C[E] :| MinLength[2]

  /** Typeclass instances and convenience syntax for working with containers of at least two
    * elements
    */
  object AtLeast2:
    /** Build an at-least-two-element list by prepending the given single element to the given
      * collection.
      *
      * Note that while this is like a "cons" operation on the underlying list, the type signature
      * is more restrictive than with that operation as typically defined. Specifically, this
      * function's type signature restricts the element being prepended to have the same type as the
      * elements already in the list, forbidding the type-expanding or type-widening operation
      * that's possible with an ordinary list prepend.
      *
      * @tparam X
      *   The type of value stored in the collection
      * @param x
      *   The element to prepend to the collection
      * @param xs
      *   The existing nonempty collection
      */
    def apply[X](x: X, xs: NonEmptyList[X]): AtLeast2[List, X] =
      (x :: xs).toList.refineUnsafe[MinLength[2]]

    /** Build an at-least-two-element list by adding the given single element to the given
      * collection.
      *
      * @tparam X
      *   The type of value stored in the collection
      * @param xs
      *   The existing nonempty collection
      * @param x
      *   The element to prepend to the collection
      */
    def apply[X](xs: NonEmptySet[X], x: X): AtLeast2[Set, X] =
      xs.add(x).toSortedSet.refineUnsafe

    /** Attempt to refine the given collection as one of at least two elements.
      *
      * @tparam C
      *   The container type of the given collection
      * @tparam X
      *   The element type of the given collection
      * @params
      *   xs The existing collection to attempt to refine as containing at at least two elements
      */
    inline def either[C[*], X](xs: C[X])(using
        inline: Constraint[C[X], MinLength[2]]
    ): Either[String, AtLeast2[C, X]] =
      xs.refineEither[MinLength[2]]

    extension [X](xs: AtLeast2[Set, X])
      /** Add an element to the given collection, usign the same definition of this operator as for
        * the underlying collection.
        *
        * @param x
        *   The element to add to the collection
        * @return
        *   The collection with the given element added
        */
      infix def +(x: X): AtLeast2[Set, X] =
        (xs + x).refineUnsafe[MinLength[2]]

    // A one-argument (element type) type constructor, fixing the container type
    private type AtLeast2FixedC[C[*]] = [X] =>> AtLeast2[C, X]

    /** Provide a [[cats.Functor]] instance using the natural definition following from an available
      * instance for the underlying container type.
      */
    given functorForAtLeast2[C[*]: Functor]: Functor[AtLeast2FixedC[C]] with
      override def map[A, B](fa: AtLeast2FixedC[C][A])(f: A => B): AtLeast2FixedC[C][B] =
        fa.map(f)
  end AtLeast2

  extension [A](bag: Set[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)

  extension [A](bag: NonEmptySet[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)

end collections
