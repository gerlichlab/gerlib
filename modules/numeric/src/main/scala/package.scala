package at.ac.oeaw.imba.gerlich.gerlib

import scala.util.Try
import cats.*
import cats.syntax.all.*
import mouse.boolean.*

/** Numeric tools and types */
package object numeric:

    /**
      * Evidence that type {@code A} has a maximum value. For coherence, require [[cats.kernel.Order]] instance.
      * 
      * @tparam A The type for which we can generate the maximum value
      */
    trait AdmitsMaximum[A : Order]:
        def maximum: MaximumValue[A]

    /** Instances of maximum admission for some types */
    object AdmitsMaximum:
        given admitsMaximumForInt: AdmitsMaximum[Int] with
            def maximum: MaximumValue[Int] = Int.MaxValue
        given admitsMaximumForNonnegativeInt: AdmitsMaximum[NonnegativeInt] with
            def maximum: MaximumValue[NonnegativeInt] = MaximumValue.ForNonnegative

    /**
      * Evidence that type {@code A} has a minimum value. For coherence, require [[cats.kernel.Order]] instance.
      * 
      * @tparam A The type for which we can generate the minimum value
      */
    trait AdmitsMinimum[A : Order]:
        def minimum: MinimumValue[A]

    /** Instances of minimum admission for some types */
    object AdmitsMinimum:
        given admitsMinimumForInt: AdmitsMinimum[Int] with
            def minimum: MinimumValue[Int] = Int.MinValue
        given admitsMinimumForNonnegativeInt: AdmitsMinimum[NonnegativeInt] with
            def minimum: MinimumValue[NonnegativeInt] = MinimumValue.ForNonnegative
    
    trait MaximumSeekingMonoid[A] extends Monoid[A]

    object MaximumSeekingMonoid:
        def instance[A : AdmitsMinimum : Order]: MaximumSeekingMonoid[A] = new:
            override def empty: A = summon[AdmitsMinimum[A]].minimum
            override def combine(x: A, y: A): A = if y > x then y else x

    opaque type MaximumValue[A] = A & Singleton

    object MaximumValue:
        private inline def apply[A : Order](a: A): MaximumValue[A] = a
        private[numeric] val ForNonnegative: MaximumValue[NonnegativeInt] = apply(NonnegativeInt(Int.MaxValue))
        extension [A](maxA: MaximumValue[A])
            def asBaseTypeValue: A = maxA
    end MaximumValue
    
    opaque type MinimumValue[A] = A & Singleton

    object MinimumValue:
        private inline def apply[A : Order](a: A): MinimumValue[A] = a
        private[numeric] val ForNonnegative: MinimumValue[NonnegativeInt] = apply(NonnegativeInt(0))
        extension [A](minA: MinimumValue[A])
            def asBaseTypeValue: A = minA
    end MinimumValue

    /** Refinement type for nonnegative integers */
    opaque type NonnegativeInt <: Int = Int
    
    /** Helpers for working with nonnegative integers */
    object NonnegativeInt:
        /**
          * Error subtype for when refinement of a negative integer as nonnegative is attempted
          *
          * @param getInt The integer to refine as nonnegative
          * @param context The context (e.g., purpose) in which the refinement's being attempted; 
          *     this is used to craft a more informative error message
          */
        final case class IllegalRefinement private[NonnegativeInt](getInt: Int, context: Option[String])
            extends NumberFormatException(s"Cannot refine $getInt as nonnegative.${context.fold("")(ctx => s" Context: $ctx")}")

        object IllegalRefinement:
            def apply(z: Int): IllegalRefinement = new IllegalRefinement(z, None)
            def apply(z: Int, ctx: String): IllegalRefinement = new IllegalRefinement(z, ctx.some)

        inline def apply(z: Int): NonnegativeInt = 
            inline if z < 0 then compiletime.error("Negative integer where nonnegative integer is required!")
            else (z: NonnegativeInt)
        
        /** Allow for downcasting nonnegative integer to simple integer. */
        extension (n: NonnegativeInt)
            def asInt: Int = n
        
        def either(z: Int): Either[String, NonnegativeInt] = maybe(z).toRight(s"Cannot refine as nonnegative integer: $z")
        
        def indexed[A](as: List[A]): List[(A, NonnegativeInt)] = as.zipWithIndex.map{ (a, i) => a -> unsafe(i) }
        
        def maybe(z: Int): Option[NonnegativeInt] = (z >= 0).option{ (z: NonnegativeInt) }
        
        def orError(z: Int): Either[IllegalRefinement, NonnegativeInt] = maybe(z).toRight(IllegalRefinement(z))
        
        def orError(context: String)(z: Int): Either[IllegalRefinement, NonnegativeInt] = maybe(z).toRight(IllegalRefinement(z, context))
        
        def parse(s: String): Either[String, NonnegativeInt] = readAsInt(s).flatMap(either)
        
        def unsafe(z: Int): NonnegativeInt = orError(z).fold(throw _, identity)
        
        given nonnegativeIntOrder(using intOrd: Order[Int]): Order[NonnegativeInt] = intOrd.contramap(identity)
        
        given numericForNonnegativeInt: Numeric[NonnegativeInt] = summon[Numeric[Int]]

        given showForNonnegativeInt: Show[NonnegativeInt] = Show.fromToString[NonnegativeInt]
    end NonnegativeInt

    /** Nonnegative real number */
    opaque type NonnegativeReal <: Double = Double

    /** Helpers for working with nonnegative real numbers */
    object NonnegativeReal:
        def either(x: Double): Either[String, NonnegativeReal] = 
            (x >= 0).either(s"Cannot refine as nonnegative number: $x", x : NonnegativeReal)
        def maybe(x: Double): Option[NonnegativeReal] = either(x).toOption
        def parse(s: String): Either[String, NonnegativeReal] = readAsDouble(s).flatMap(either)
        given numericForNonnegativeReal: Numeric[NonnegativeReal] = summon[Numeric[Double]]
    end NonnegativeReal

    /** Refinement type for positive integers */
    opaque type PositiveInt <: NonnegativeInt = Int

    /** Helpers for working with positive integers */
    object PositiveInt:
        extension (n: PositiveInt)
            def asInt: Int = n
            def asNonnegativeInt: NonnegativeInt = n
        inline def apply(z: Int): PositiveInt = 
            inline if z <= 0 then compiletime.error("Non-positive integer where positive integer is required!")
            else (z: PositiveInt)
        def either(z: Int): Either[String, PositiveInt] = maybe(z).toRight(s"Cannot refine as positive integer: $z")
        def maybe(z: Int): Option[PositiveInt] = (z > 0).option{ (z: PositiveInt) }
        def unsafe(z: Int): PositiveInt = either(z).fold(msg => throw new NumberFormatException(msg), identity)
        given nonnegativeIntOrder(using intOrd: Order[Int]): Order[PositiveInt] = intOrd.contramap(identity)
        given showForPositiveInt: Show[PositiveInt] = Show.fromToString[PositiveInt]
    end PositiveInt

    /** Positive real number */
    opaque type PositiveReal <: NonnegativeReal = Double

    /** Helpers for working with positive real numbers */
    object PositiveReal:
        inline def apply(x: Double): PositiveReal = 
            inline if x <= 0 then compiletime.error("Non-positive number where positive number is required!")
            else (x: PositiveReal)
        def either(x: Double): Either[String, PositiveReal] = 
            (x > 0).either(s"Cannot refine as positive number: $x", x : PositiveReal)
        def maybe(x: Double): Option[PositiveReal] = either(x).toOption
        def parse(s: String): Either[String, PositiveReal] = 
            Try{ s.toDouble }
                .toEither
                .leftMap(e => s"Cannot convert value ($s) to numeric: ${e.getMessage}")
                .flatMap(either)
    end PositiveReal

    def readAsInt(s: String): Either[String, Int] =
        Try{ s.toInt }.toEither.leftMap(e => s"Cannot convert given value to integer: $s")

    def readAsDouble(s: String): Either[String, Double] =
        Try{ s.toDouble }.toEither.leftMap(e => s"Cannot convert given value to double: $s")

    /**
      * Try to read a string into a target type, through nonnegative integer intermediate.
      *
      * @param aName The name of the target type, used to contextualise error message
      * @param lift How to lift raw nonnegative integer into target type
      * @return Either a [[scala.util.Left]]-wrapped error message, or a [[scala.util.Right]]-wrapped 
      *     parsed value
      */
    def parseThroughNonnegativeInt[A](aName: String)(lift: NonnegativeInt => A): String => Either[String, A] = s =>
        NonnegativeInt.parse(s).bimap(msg => s"For $aName -- $msg", lift)

end numeric
