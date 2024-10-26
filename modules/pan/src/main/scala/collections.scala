package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.data.{NonEmptyList, NonEmptySet}
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
  opaque type AtLeast2[C[*], E] = C[E] :| MinLength[2]

  /** A one-argument (element type) type constructor, fixing the container type */
  private[gerlib] type AtLeast2FixedC[C[*]] = [X] =>> AtLeast2[C, X]

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

    inline def unsafe[C[*], X](xs: C[X])(using
        inline ev: Constraint[C[X], MinLength[2]]
    ): AtLeast2[C, X] =
      xs.refineUnsafe

    /** Define equality the same way as for the underlying, unrefined value. */
    given eqForAtLeast2[C[*], E](using Eq[C[E]]): Eq[AtLeast2[C, E]] = Eq.by(es => es: C[E])

    /** Syntax enrichment for certain type members of at AtLeast2 family */
    object syntax:
      extension [X](xs: AtLeast2[Set, X])
        /** Add an element to the given collection, usign the same definition of this operator as
          * for the underlying collection.
          *
          * @param x
          *   The element to add to the collection
          * @return
          *   The collection with the given element added
          */
        infix def +(x: X): AtLeast2[Set, X] =
          (xs + x).refineUnsafe[MinLength[2]]

      extension [X](xs: AtLeast2[List, X])
        /** With knowledge that the given container type is an set, we can use the underlying
          * collection's {@code .contains} member.
          *
          * @return
          *   Whether the underlying collection contains the given element
          */
        def contains(x: X): Boolean = (xs: List[X]).contains(x)

        /** We can safely convert to [[cats.data.NonEmptyList]]. */
        def toNel: NonEmptyList[X] = NonEmptyList(xs.head, xs.tail)

      extension [C[*] <: Set[*], X](xs: AtLeast2[C, X])
        /** With knowledge that the given container type is an set, we can use the underlying
          * collection's {@code .contains} member.
          *
          * @return
          *   Whether the underlying collection contains the given element
          */
        def contains(x: X): Boolean = (xs: C[X]).contains(x)

      extension [C[*], X](xs: AtLeast2[C, X])
        /** When the underlying container type has a functor, use it to {@code .map} over the
          * refined collection.
          *
          * @tparam Y
          *   The codomain
          * @param f
          *   The function to apply to each value of the collection
          * @param F
          *   The [[cats.Functor]] instance for the underlying container type
          * @param constraint
          *   Proof of the requisite constraint for the output collection
          * @return
          *   A length-refined collection with each value mapped according to the given
          *   transformation
          */
        inline def map[Y](
            f: X => Y
        )(using F: Functor[C], inline constraint: Constraint[C[Y], MinLength[2]]): AtLeast2[C, Y] =
          unsafe(F.map(xs: C[X])(f))

      extension [C[*] <: Iterable[*], X](xs: AtLeast2[C, X])
        /** With knowledge that the given container type is an iterable, we can use the underlying
          * collection's {@code .length} member.
          *
          * @return
          *   The size of the underlying collection
          */
        def size: Int = (xs: C[X]).size

        /** Library-private access to the underlying iterable */
        private[gerlib] def toNativeIterable: C[X] = xs: C[X]

        /** With knowledge that the container is iterable, use the underlying collection's
          * {@code .toList} method.
          *
          * @return
          *   A list representation of the underlying collection
          */
        def toList: List[X] = (xs: C[X]).toList

        /** With knowledge that the underlying container is iterable, convert the underlying
          * collection to a set.
          *
          * @return
          *   A set representation of the underlying collection
          */
        def toSet: Set[X] = (xs: C[X]).toSet
    end syntax
  end AtLeast2

  extension [A](bag: Set[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)

  extension [A](bag: NonEmptySet[A])
    /** Negation of inclusion/membership test result */
    def excludes(a: A): Boolean = !bag.contains(a)

end collections
