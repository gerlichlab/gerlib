package at.ac.oeaw.imba.gerlich.gerlib.numeric

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import at.ac.oeaw.imba.gerlich.gerlib.testing.ScalacheckSuite

/** Tests for positive integer refinement type */
class TestNonnegativeInt extends AnyFunSuite, should.Matchers, ScalacheckSuite:
    test("NonnegativeInt correctly restricts which expressions compile.") {
        assertCompiles("NonnegativeInt(1)")
        assertCompiles("NonnegativeInt(0)")
        assertTypeError("NonnegativeInt(-1)")
    }

    test("NonnegativeInt.maybe behaves correctly.") {
        forAll { (z: Int) => NonnegativeInt.maybe(z) match {
            case None if z < 0 => succeed
            case Some(n) if z >= 0 => z shouldEqual n
            case bad => fail(s"NonnegativeInt.maybe($z) gave bad result: $bad")
        } }
    }

    test("NonnegativeInt.unsafe behaves in accordance with its safe counterpart.") {
        forAll { (z: Int) => NonnegativeInt.maybe(z) match {
            case None => assertThrows[NumberFormatException]{ NonnegativeInt.unsafe(z) }
            case Some(n) => n shouldEqual NonnegativeInt.unsafe(z)
        } }
    }
end TestNonnegativeInt
