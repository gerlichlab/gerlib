package at.ac.oeaw.imba.gerlich.gerlib.numeric

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for positive integer refinement type */
class TestPositiveInt extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100)

    test("PositiveInt correctly restricts which expressions compile.") {
        // NB: autoRefine is required for this (raw, non-constructor) syntax of type refinement.
        assertCompiles("import io.github.iltotore.iron.autoRefine; val one: PositiveInt = 1")
        assertTypeError("val one: PositiveInt = 1")
        assertTypeError("val zero: PositiveInt = 0")
        assertTypeError("import io.github.iltotore.iron.autoRefine; val zero: PositiveInt = 0")
        assertTypeError("val moinsDeux: PositiveInt = -2")
    }

    test("PositiveInt correctly restricts which expressions compile by constructor syntax.") {
        // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
        assertCompiles("PositiveInt(1)")
        assertTypeError("PositiveInt(0)")
        assertTypeError("PositiveInt(-2)")
    }

    test("PositiveInt.maybe behaves correctly.") {
        forAll { (z: Int) => PositiveInt.maybe(z) match {
            case None if z <= 0 => succeed
            case Some(n) if z > 0 => z shouldEqual n
            case bad => fail(s"PositiveInt.maybe($z) gave bad result: $bad")
        } }
    }

    test("PositiveInt.unsafe behaves in accordance with its safe counterpart.") {
        forAll { (z: Int) => PositiveInt.either(z) match {
            case Left(_) => assertThrows[IllegalRefinement[Int]]{ PositiveInt.unsafe(z) }
            case Right(n) => n shouldEqual PositiveInt.unsafe(z)
        } }
    }
end TestPositiveInt
