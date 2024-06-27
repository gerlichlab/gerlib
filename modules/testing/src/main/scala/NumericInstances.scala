package at.ac.oeaw.imba.gerlich.gerlib.testing

import org.scalacheck.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.testing.GeneratorBound.{ Lower as LowerBound, Upper as UpperBound }

/** Tools for writing property-based tests involving custom numeric types */
trait NumericInstances:
    /** Choose a nonnegative integer through the Choose[Int], then unsafely refine */
    given chooseForNonnegativeInt: Gen.Choose[NonnegativeInt] with
        override def choose(min: NonnegativeInt, max: NonnegativeInt): Gen[NonnegativeInt] = 
            Gen.choose(min.asInt, max.asInt).map{ NonnegativeInt.unsafe }

    /** [[org.scalacheck.Arbitrary]] instance for generating nonnegative integer, subject to the given bounds */
    given arbitraryForNonnegativeInt(using lo: LowerBound[NonnegativeInt], hi: UpperBound[NonnegativeInt]): Arbitrary[NonnegativeInt] = 
        Arbitrary{ Gen.choose(lo.value, hi.value) }
end NumericInstances
