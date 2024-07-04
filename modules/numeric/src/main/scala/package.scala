package at.ac.oeaw.imba.gerlich.gerlib

import scala.util.Try
import cats.*
import cats.syntax.all.*
import mouse.boolean.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.given
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.* // for summoning Order[(Int | Double) :| P]
import io.github.iltotore.iron.macros.assertCondition

/** Numeric tools and types */
package object numeric:
    /**
      * Evidence that a value is integer-like
      * 
      * @tparam A The type of value which is integer-like
      */
    trait IntLike[A]:
        /** Get an integer from the given value. */
        def asInt: A => Int

    /** Instances of the trait */
    object IntLike:
        /** IntLike for Int is identity. */
        given IntLike[Int] with
            def asInt: Int => Int = identity
        /** Syntax */
        extension [A](a: A)(using ev: IntLike[A])
            def asInt: Int = ev.asInt(a)
        /** Transitive Int fetching */
        given Contravariant[IntLike] with
            def contramap[A, B](fa: IntLike[A])(f: B => A): IntLike[B] = new:
                def asInt: B => Int = f `andThen` fa.asInt

    /**
     * Error subtype for when refinement of a negative integer as nonnegative is attempted
     *
     * @param getInt The integer to refine as nonnegative
     * @param context The context (e.g., purpose) in which the refinement's being attempted;
     *     this is used to craft a more informative error message
     */
    final case class IllegalRefinement[A] private[numeric](rawValue: A, msg: String, context: Option[String])
        extends IllegalArgumentException(
            s"Cannot refine value $rawValue: $msg.${context.fold("")(ctx => s" Context: $ctx")}"
        )

    /** Builders for [[IllegalRefinement]] */
    object IllegalRefinement:
        /** No additional message context */
        private[numeric] def apply[A](value: A, msg: String): IllegalRefinement[A] = 
            new IllegalRefinement(value, msg, None)
        /** Add additional message context. */
        private[numeric] def apply[A](value: A, msg: String, ctx: String): IllegalRefinement[A] = 
            new IllegalRefinement(value, msg, ctx.some)
    
    /**
      * Enrich [[io.github.iltotore.iron.RefinedTypeOps]] with semantic context for error messages, and a parser.
      * 
      * @tparam V The type of value to refine with constraint/predicate
      * @tparam P The constraint/predicate by which to refine values
      * @see [[io.github.iltotore.iron.RefinedTypeOps]]
      */
    private sealed trait RefinementBuilder[V, P] extends RefinedTypeOps.Transparent[V :| P]:
        /**
         * Compile-time refinement of a {@code V} acc. to predicate {@code P}, with constructor-like syntax
         * 
         * @param v The value to test under predicate/constraint {@code P}
         * @param constraint The predicate under which to test a given value
         * @see [[io.github.iltotore.iron.autoRefine]]
         */
        inline def apply(inline v: V)(using inline constraint: Constraint[V, P]): V :| P = 
            assertCondition(v, constraint.test(v), constraint.message)
            IronType(v)
        /** How to safely try to get a raw value {@code V} from text */
        protected def parseRaw: String => Either[String, V]
        /** Alias for {@code option} */
        final def maybe(v: V): Option[V :| P] = option(v)
        /** Provide a safe parser of {@code V :| P} from text */
        def parse: String => Either[String, V :| P] = parseRaw.map(_.flatMap(either))
        /** Modify the parent trait's {@code either} member by injecting semantic context into fail message. */
        final def unsafe: V => V :| P = v => either(v).fold(msg => throw IllegalRefinement(v, msg), identity)

    /** Refinement type for nonnegative integers */
    type NonnegativeInt = Int :| Not[Negative]

    /** Helpers for working with nonnegative integers */
    object NonnegativeInt extends RefinementBuilder[Int, Not[Negative]]:
        override protected def parseRaw: String => Either[String, Int] = readAsInt
        def indexed[A](as: List[A]): List[(A, NonnegativeInt)] = as.zipWithIndex.map{ (a, i) => a -> unsafe(i) }
        given IntLike[NonnegativeInt] with
            override def asInt = identity
        given Order[NonnegativeInt] = summon[Order[NonnegativeInt]]
        given Show[NonnegativeInt] = summon[Show[NonnegativeInt]]
    end NonnegativeInt

    /** Nonnegative real number */
    type NonnegativeReal = Double :| Not[Negative]

    /** Helpers for working with nonnegative real numbers */
    object NonnegativeReal extends RefinementBuilder[Double, Not[Negative]]:
        override protected def parseRaw: String => Either[String, Double] = readAsDouble
        given orderForNonnegativeReal: Order[NonnegativeReal] = summon[Order[NonnegativeReal]]
        given showForNonnegativeReal(using ev: Show[Double]): Show[NonnegativeReal] = 
            ev.contramap(identity)
        given Subtraction[NonnegativeReal, Double, Double] with
            def minus(minuend: NonnegativeReal)(subtrahend: Double): Double = minuend - subtrahend
    end NonnegativeReal

    /** Refinement type for positive integers */
    type PositiveInt = Int :| Positive

    /** Helpers for working with positive integers */
    object PositiveInt extends RefinementBuilder[Int, Positive]:
        override protected def parseRaw: String => Either[String, Int] = readAsInt
        given IntLike[PositiveInt] with
            override def asInt = identity
        given Order[PositiveInt] = summon[Order[PositiveInt]]
        given Show[PositiveInt] = summon[Show[PositiveInt]]
    end PositiveInt

    /** Positive real number */
    type PositiveReal = Double :| Positive

    /** Helpers for working with positive real numbers */
    object PositiveReal extends RefinementBuilder[Double, Positive]:
        override protected def parseRaw: String => Either[String, Double] = readAsDouble
        given Order[PositiveReal] = summon[Order[PositiveReal]]
        given showForPositiveReal(using ev: Show[Double]): Show[PositiveReal] = 
            ev.contramap(identity)
        given Subtraction[PositiveReal, Double, Double] with
            def minus(minuend: PositiveReal)(subtrahend: Double): Double = minuend - subtrahend
    end PositiveReal

    /** Attempt to parse the given text as integer, wrapping error message as a [[scala.util.Left]] for fail. */
    def readAsInt(s: String): Either[String, Int] =
        Try{ s.toInt }.toEither.leftMap(e => s"Cannot read as integer: $s")

    /** Attempt to parse the given text as decimal, wrapping error message as a [[scala.util.Left]] for fail. */
    def readAsDouble(s: String): Either[String, Double] =
        Try{ s.toDouble }.toEither.leftMap(e => s"Cannot read as double: $s")

    /**
      * Try to read a string into a target type, through nonnegative integer intermediate.
      *
      * @tparam T The target type
      * @param aName The name of the target type, used to contextualise error message
      * @param lift How to lift raw nonnegative integer into target type
      * @return Either a [[scala.util.Left]]-wrapped error message, or a [[scala.util.Right]]-wrapped 
      *     parsed value
      */
    def parseThroughNonnegativeInt[T](aName: String)(lift: NonnegativeInt => T): String => Either[String, T] = 
        s => NonnegativeInt.parse(s).bimap(msg => s"For $aName -- $msg", lift)

end numeric
