package at.ac.oeaw.imba.gerlich.gerlib

import cats.data.*
import cats.laws.discipline.arbitrary.given // for Arbitrary[NonEmptySet[*]], Arbitrary[NonEmptyList[*]], etc.
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2

/** Tests for the [[collections.AtLeast2]] refinement type
  */
class TestAtLeast2 extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:

  test("AtLeast2[Set, X] is correct with apply-syntax.") {
    forAll: (xs: NonEmptySet[Int], x: Int) =>
      val atLeast2 = AtLeast2(xs, x)
      atLeast2 `contains` x shouldBe true
      atLeast2.size shouldBe (xs.length + (if xs `contains` x then 0 else 1))
  }

  test("AtLeast2.apply only compiles for set-then-element argument order.") {
    assertTypeError:
      "AtLeast2(1, NonEmptySet.one(0))"
    assertCompiles:
      "AtLeast2(NonEmptySet.one(0), 1)"
  }

end TestAtLeast2
