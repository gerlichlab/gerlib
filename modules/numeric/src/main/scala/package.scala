package at.ac.oeaw.imba.gerlich.gerlib

import scala.util.Try
import cats.*
import cats.data.Kleisli
import cats.syntax.all.*
import mouse.boolean.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.given
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.* // for summoning Order[(Int | Double) :| P]

/** Numeric tools and types */
package object numeric:
    /**
     * Error subtype for when refinement of a negative integer as nonnegative is attempted
     *
     * @param getInt The integer to refine as nonnegative
     * @param context The context (e.g., purpose) in which the refinement's being attempted;
     *     this is used to craft a more informative error message
     */
    final case class IllegalRefinement[A] private[numeric](rawValue: A, msg: String, context: Option[String])
        extends NumberFormatException(s"Cannot refine value $rawValue: $msg.${context.fold("")(ctx => s" Context: $ctx")}")

    object IllegalRefinement:
        private[numeric] def apply[A](value: A, msg: String): IllegalRefinement[A] = new IllegalRefinement(value, msg, None)
        private[numeric] def apply[A](value: A, msg: String, ctx: String): IllegalRefinement[A] = new IllegalRefinement(value, msg, ctx.some)
    
    private sealed trait RefinementBuilder[V, P]:
        protected def context: String
        protected def parseRaw: String => Either[String, V]
        protected def liftRaw: V => Either[String, V :| P]
        final def either: V => Either[String, V :| P] = liftRaw
        def maybe: V => Option[V :| P] = either.map(_.toOption)
        def parse: String => Either[String, V :| P] = parseRaw.map(_.flatMap(either))
        def unsafe: V => V :| P = v => either(v).fold(msg => throw IllegalRefinement(v, msg, context), identity)

    /** Refinement type for nonnegative integers */
    type NonnegativeInt = Int :| Not[Negative]

    /** Helpers for working with nonnegative integers */
    object NonnegativeInt extends RefinementBuilder[Int, Not[Negative]]:
        override protected def context: String = "nonnegative integer"
        override protected def parseRaw: String => Either[String, Int] = readAsInt
        override protected def liftRaw: Int => Either[String, NonnegativeInt] = _.refineEither[Not[Negative]]
        def indexed[A](as: List[A]): List[(A, NonnegativeInt)] = as.zipWithIndex.map{ (a, i) => a -> unsafe(i) }
        given orderForNonnegativeInt: Order[NonnegativeInt] = summon[Order[NonnegativeInt]]
    end NonnegativeInt

    /** Nonnegative real number */
    type NonnegativeReal = Double :| Not[Negative]

    /** Helpers for working with nonnegative real numbers */
    object NonnegativeReal extends RefinementBuilder[Double, Not[Negative]]:
        override protected def context: String = "nonnegative real"
        override protected def parseRaw: String => Either[String, Double] = readAsDouble
        override def liftRaw: Double => Either[String, NonnegativeReal] = _.refineEither[Not[Negative]]
    end NonnegativeReal

    /** Refinement type for positive integers */
    type PositiveInt = Int :| Positive

    /** Helpers for working with positive integers */
    object PositiveInt extends RefinementBuilder[Int, Positive]:
        override protected def context: String = "positive integer"
        override protected def parseRaw: String => Either[String, Int] = readAsInt
        override def liftRaw: Int => Either[String, PositiveInt] = _.refineEither[Positive]
        given orderForPositiveInt: Order[PositiveInt] = summon[Order[PositiveInt]]
    end PositiveInt

    /** Positive real number */
    type PositiveReal = Double :| Positive

    /** Helpers for working with positive real numbers */
    object PositiveReal extends RefinementBuilder[Double, Positive]:
        override protected def context: String = "positive real"
        override protected def parseRaw: String => Either[String, Double] = readAsDouble
        override def liftRaw: Double => Either[String, PositiveReal] = _.refineEither[Positive]
    end PositiveReal

    def readAsInt(s: String): Either[String, Int] =
        Try{ s.toInt }.toEither.leftMap(e => s"Cannot read as integer: $s")

    def readAsDouble(s: String): Either[String, Double] =
        Try{ s.toDouble }.toEither.leftMap(e => s"Cannot read as double: $s")

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
