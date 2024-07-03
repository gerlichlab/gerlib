package at.ac.oeaw.imba.gerlich.gerlib.numeric

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for positive integer refinement type */
class TestNonnegativeInt extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100)

    test("NonnegativeInt correctly restricts which expressions compile by direct assignment.") {
        // NB: autoRefine is required for this (raw, non-constructor) syntax of type refinement.
        assertCompiles("import io.github.iltotore.iron.autoRefine; val one: NonnegativeInt = 1")
        assertTypeError("val one: NonnegativeInt = 1")
        assertCompiles("import io.github.iltotore.iron.autoRefine; val zero: NonnegativeInt = 0")
        assertTypeError("val moinsDeux: NonnegativeInt = -2")
    }

    test("NonnegativeInt correctly restricts which expressions compile by constructor syntax.") {
        // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
        assertCompiles("NonnegativeInt(1)")
        assertCompiles("NonnegativeInt(0)")
        assertTypeError("NonnegativeInt(-2)")
    }

    test("NonnegativeInt.maybe behaves correctly.") {
        forAll { (z: Int) => NonnegativeInt.maybe(z) match {
            case None if z < 0 => succeed
            case Some(n) if z >= 0 => z shouldEqual n
            case bad => fail(s"NonnegativeInt.maybe($z) gave bad result: $bad")
        } }
    }

    test("NonnegativeInt.unsafe behaves in accordance with its safe counterpart.") {
        forAll { (z: Int) => NonnegativeInt.either(z) match {
            case Left(_) => assertThrows[IllegalRefinement[Int]]{ NonnegativeInt.unsafe(z) }
            case Right(n) => n shouldEqual NonnegativeInt.unsafe(z)
        } }
    }
end TestNonnegativeInt
