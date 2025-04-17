package at.ac.oeaw.imba.gerlich.gerlib

import scala.util.Try
import cats.*
import cats.data.NonEmptyList
import cats.syntax.all.*

import io.github.iltotore.iron.RefinedType
import io.github.iltotore.iron.constraint.any.Not
// for summoning Order[Int :| P] or Order[Double :| P]
import io.github.iltotore.iron.constraint.numeric.*

import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

/** Numeric tools and types */
package object numeric:
  /** Evidence that a value is integer-like
    *
    * @tparam A
    *   The type of value which is integer-like
    */
  trait IntLike[A]:
    /** Get an integer from the given value. */
    def asInt: A => Int

  /** Instances of the trait */
  object IntLike:
    /** IntLike for Int is identity. */
    given IntLike[Int]:
      def asInt: Int => Int = identity

    /** Syntax */
    extension [A](a: A)(using ev: IntLike[A]) def asInt: Int = ev.asInt(a)

    /** Transitive Int fetching */
    given Contravariant[IntLike]:
      def contramap[A, B](fa: IntLike[A])(f: B => A): IntLike[B] = new:
        def asInt: B => Int = f `andThen` fa.asInt

  /** Refinement type for nonnegative integers */
  type NonnegativeInt = NonnegativeInt.T

  /** Helpers for working with nonnegative integers */
  object NonnegativeInt extends RefinedType[Int, Not[Negative]]:
    def indexed[F[_]: Functor, A](ais: F[(A, Int)]): F[(A, NonnegativeInt)] =
      ais.map((a, i) =>
        a -> NonnegativeInt.either(i).fold(msg => throw IllegalRefinement(i, msg), identity)
      )
    
    def indexed[A](as: List[A]): List[(A, NonnegativeInt)] = indexed(as.zipWithIndex)

    def indexed[A](as: NonEmptyList[A]): NonEmptyList[(A, NonnegativeInt)] = indexed(as.zipWithIndex)
  end NonnegativeInt

  /** Nonnegative real number */
  type NonnegativeReal = NonnegativeReal.T

  /** Helpers for working with nonnegative real numbers */
  object NonnegativeReal extends RefinedType[Double, Not[Negative]]

  /** Refinement type for positive integers */
  type PositiveInt = PositiveInt.T

  /** Helpers for working with positive integers */
  object PositiveInt extends RefinedType[Int, Positive]

  /** Positive real number */
  type PositiveReal = PositiveReal.T

  /** Helpers for working with positive real numbers */
  object PositiveReal extends RefinedType[Double, Positive]

  /** Attempt to parse the given text as integer, wrapping error message as a [[scala.util.Left]]
    * for fail.
    */
  def readAsInt(s: String): Either[String, Int] =
    Try { s.toInt }.toEither.leftMap(e => s"Cannot read as integer: $s")

  /** Attempt to parse the given text as decimal, wrapping error message as a [[scala.util.Left]]
    * for fail.
    */
  def readAsDouble(s: String): Either[String, Double] =
    Try { s.toDouble }.toEither.leftMap(e => s"Cannot read as double: $s")

  /** Try to read a string into a target type, through nonnegative integer intermediate.
    *
    * @tparam T
    *   The target type
    * @param targetName
    *   The name of the target type, used to contextualise error message
    * @param lift
    *   How to lift raw nonnegative integer into target type
    * @return
    *   Either a [[scala.util.Left]]-wrapped error message, or a [[scala.util.Right]]-wrapped parsed
    *   value
    */
  def parseThroughNonnegativeInt[T](targetName: String)(
      lift: NonnegativeInt => T
  ): String => Either[String, T] = s =>
    readAsInt(s)
      .flatMap(NonnegativeInt.either)
      .bimap(msg => s"For $targetName -- $msg", lift)

end numeric
