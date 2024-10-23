package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import org.scalacheck.{Arbitrary, Gen}

import at.ac.oeaw.imba.gerlich.gerlib.testing.syntax.scalacheck.*

/** [[cats]] instances for [[org.scalacheck]] data types */
trait CatsScalacheckInstances:

  /** Define {@code Applicative[Arbitrary]} i.t.o. {@code Applicative[Gen]}. */
  given applicativeForArbitrary(using
      ev: Applicative[Gen]
  ): Applicative[Arbitrary] with
    override def pure[A](a: A): Arbitrary[A] = ev.pure(a).toArbitrary
    override def ap[A, B](ff: Arbitrary[A => B])(
        fa: Arbitrary[A]
    ): Arbitrary[B] =
      ev.ap(ff.arbitrary)(fa.arbitrary).toArbitrary

  /** Use Gen.flatMap to define {@code Applicative.ap} , and {@code Gen.const} to define
    * {@code Applicative.pure} .
    */
  given applicativeForGen: Applicative[Gen] with
    override def pure[A](a: A) = Gen.const(a)
    override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = for
      f <- ff
      a <- fa
    yield f(a)

  /** Use [[org.scalacheck.Gen]]'s {@code nonEmptyListOf} member to build a
    * [[cats.data.NonEmptyList]]
    */
  given arbitraryForNonEmptyList[A: Arbitrary](using
      ev: Arbitrary[List[A]]
  ): Arbitrary[NonEmptyList[A]] =
    Gen
      .nonEmptyListOf(Arbitrary.arbitrary[A])
      .map(_.toNel)
      .map(_.getOrElse {
        throw new Exception(
          "Generated empty list when using Gen.nonEmptyListOf!"
        )
      })
      .toArbitrary
end CatsScalacheckInstances
