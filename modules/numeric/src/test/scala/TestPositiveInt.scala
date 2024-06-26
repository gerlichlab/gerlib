package at.ac.oeaw.imba.gerlich.gerlib.numeric

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import at.ac.oeaw.imba.gerlich.gerlib.testing.ScalacheckSuite

/** Tests for positive integer refinement type */
class TestPositiveInt extends AnyFunSuite, should.Matchers, ScalacheckSuite:
    test("PositiveInt correctly restricts which expressions compile.") {
        assertCompiles("PositiveInt(1)")
        assertTypeError("PositiveInt(0)")
        assertTypeError("PositiveInt(-1)")
    }

    test("PositiveInt.maybe behaves correctly.") {
        forAll { (z: Int) => PositiveInt.maybe(z) match {
            case None if z <= 0 => succeed
            case Some(n) if z > 0 => z shouldEqual n
            case bad => fail(s"PositiveInt.maybe($z) gave bad result: $bad")
        } }
    }

    test("PositiveInt.unsafe behaves in accordance with its safe counterpart.") {
        forAll { (z: Int) => PositiveInt.maybe(z) match {
            case None => assertThrows[NumberFormatException]{ PositiveInt.unsafe(z) }
            case Some(n) => n shouldEqual PositiveInt.unsafe(z)
        } }
    }

    test("Natural numbers are a subset of nonnegative integers.") {
        forAll(Gen.posNum[Int]) {
            n => PositiveInt.unsafe(n).asNonnegativeInt shouldEqual NonnegativeInt.unsafe(n)
        }
    }
end TestPositiveInt
