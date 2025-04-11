package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import org.scalacheck.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.{Negative, Positive}
import io.github.iltotore.iron.scalacheck.numeric.intervalArbitrary

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.testing.GeneratorBound.{
  Lower as LowerBound,
  Upper as UpperBound
}
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

/** Tools for writing property-based tests involving custom numeric types */
trait NumericInstances:
  /** Choose a nonnegative integer through the Choose[Int], then unsafely refine.
    */
  given Gen.Choose[NonnegativeInt]:
    override def choose(
        min: NonnegativeInt,
        max: NonnegativeInt
    ): Gen[NonnegativeInt] =
      Gen
        .choose[Int](min, max)
        .map: i =>
          NonnegativeInt.either(i).fold(msg => throw IllegalRefinement(i, msg), identity)

  /** Choose a positive integer through the Choose[Int], then unsafely refine.
    */
  given Gen.Choose[PositiveInt]:
    override def choose(min: PositiveInt, max: PositiveInt): Gen[PositiveInt] =
      Gen
        .choose[Int](min, max)
        .map: i =>
          PositiveInt.either(i).fold(msg => throw IllegalRefinement(i, msg), identity)

  /** [[org.scalacheck.Arbitrary]] instance for generating bounded numeric type, subject to the
    * given bounds.
    */
  given [V: {Gen.Choose, Numeric}, P] => (
      lo: LowerBound[V :| P],
      hi: UpperBound[V :| P]
  ) => Arbitrary[V :| P] =
    intervalArbitrary[V, P](lo.value, hi.value)

  given nonnegativeIntArbitrary: Arbitrary[Int :| Not[Negative]] =
    intervalArbitrary[Int, Not[Negative]](0, Int.MaxValue)

  given positiveIntArbitrary: Arbitrary[Int :| Positive] =
    intervalArbitrary[Int, Positive](1, Int.MaxValue)

  given nonnegativeRealArbitrary: Arbitrary[Double :| Not[Negative]] =
    intervalArbitrary[Double, Not[Negative]](0, Double.MaxValue)

  given positiveRealArbitrary: Arbitrary[Double :| Positive] =
    intervalArbitrary[Double, Positive](1 / Double.MaxValue, Double.MaxValue)
end NumericInstances
