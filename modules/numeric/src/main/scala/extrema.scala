package at.ac.oeaw.imba.gerlich.gerlib.numeric

import cats.*
import cats.syntax.all.*

object extrema:
    /** Typelevel encoding of notion that a value is a type's maximum */
    opaque type MaximumValue[A] = A & Singleton

    /** Typelevel encoding of notion that a value is a type's minimum */
    opaque type MinimumValue[A] = A & Singleton

    /**
     * Evidence that type {@code A} has a maximum value.
     * 
     * @tparam A The type for which we can generate the maximum value
     */
    trait AdmitsMaximum[A]:
        def maximum: MaximumValue[A]

    /** Instances of maximum admission for some types */
    object AdmitsMaximum:
        given admitsMaximumForInt: AdmitsMaximum[Int] with
            def maximum: MaximumValue[Int] = Int.MaxValue
        given admitsMaximumForNonnegativeInt: AdmitsMaximum[NonnegativeInt] with
            def maximum: MaximumValue[NonnegativeInt] = MaximumValue.ForNonnegative
    end AdmitsMaximum

    /**
      * Evidence that type {@code A} has a minimum value.
      * 
      * @tparam A The type for which we can generate the minimum value
      */
    trait AdmitsMinimum[A]:
        def minimum: MinimumValue[A]

    /** Instances of minimum admission for some types */
    object AdmitsMinimum:
        given admitsMinimumForInt: AdmitsMinimum[Int] with
            def minimum: MinimumValue[Int] = Int.MinValue
        given admitsMinimumForNonnegativeInt: AdmitsMinimum[NonnegativeInt] with
            def minimum: MinimumValue[NonnegativeInt] = MinimumValue.ForNonnegative
    end AdmitsMinimum

    object MaximumValue:
        private inline def apply[A](a: A): MaximumValue[A] = a
        private[extrema] val ForNonnegative: MaximumValue[NonnegativeInt] =
            apply(NonnegativeInt.unsafe(Int.MaxValue))
        extension [A](maxA: MaximumValue[A])
            def asBaseTypeValue: A = maxA
    end MaximumValue
    
    object MinimumValue:
        private inline def apply[A](a: A): MinimumValue[A] = a
        private[extrema] val ForNonnegative: MinimumValue[NonnegativeInt] = 
            apply(NonnegativeInt.unsafe(0))
        extension [A](minA: MinimumValue[A])
            def asBaseTypeValue: A = minA
    end MinimumValue

    trait MaximumSeekingMonoid[A] extends Monoid[A]

    object MaximumSeekingMonoid:
        def instance[A : AdmitsMinimum : Order]: MaximumSeekingMonoid[A] = new:
            override def empty: A = summon[AdmitsMinimum[A]].minimum
            override def combine(x: A, y: A): A = if y > x then y else x
    end MaximumSeekingMonoid
