package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import org.scalacheck.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.numeric.Positive
import io.github.iltotore.iron.scalacheck.numeric.intervalArbitrary

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.testing.GeneratorBound.{
  Lower as LowerBound,
  Upper as UpperBound
}

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
        .map:
          NonnegativeInt.unsafe

  /** Choose a positive integer through the Choose[Int], then unsafely refine.
    */
  given Gen.Choose[PositiveInt]:
    override def choose(min: PositiveInt, max: PositiveInt): Gen[PositiveInt] =
      Gen
        .choose[Int](min, max)
        .map:
          PositiveInt.unsafe

  /** [[org.scalacheck.Arbitrary]] instance for generating bounded numeric type, subject to the
    * given bounds.
    */
  given [V: {Gen.Choose, Numeric}, P] => (
      lo: LowerBound[V :| P],
      hi: UpperBound[V :| P]
  ) => Arbitrary[V :| P] =
    intervalArbitrary[V, P](lo.value, hi.value)

  given nonnegativeIntArbitrary: Arbitrary[NonnegativeInt] =
    intervalArbitrary[Int, Nonnegative](0, Int.MaxValue)

  given positiveIntArbitrary: Arbitrary[PositiveInt] =
    intervalArbitrary[Int, Positive](1, Int.MaxValue)

  given nonnegativeRealArbitrary: Arbitrary[NonnegativeReal] =
    intervalArbitrary[Double, Nonnegative](0, Double.MaxValue)

  given positiveRealArbitrary: Arbitrary[PositiveReal] =
    intervalArbitrary[Double, Positive](1 / Double.MaxValue, Double.MaxValue)
end NumericInstances
