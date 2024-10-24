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

  test("For lists, AtLeast2.apply has cons-like argument order"):
    assertTypeError:
      "AtLeast2(NonEmptyList.one(1), 0)"
    assertCompiles:
      "AtLeast2(0, NonEmptyList.one(1))"

  test("For sets, AtLeast2.apply only compiles for set-then-element argument order."):
    assertTypeError:
      "AtLeast2(1, NonEmptySet.one(0))"
    assertCompiles:
      "AtLeast2(NonEmptySet.one(0), 1)"

  test("For lists, AtLeat2 is correct with apply-syntax"):
    forAll: (xs: NonEmptyList[Int], x: Int) =>
      val atLeast2 = AtLeast2(x, xs)
      atLeast2 `contains` x shouldBe true
      atLeast2.length shouldEqual 1 + xs.length

  test("For sets, AtLeast2 is correct with apply-syntax."):
    forAll: (xs: NonEmptySet[Int], x: Int) =>
      val atLeast2 = AtLeast2(xs, x)
      atLeast2 `contains` x shouldBe true
      atLeast2.size shouldEqual (xs.length + (if xs `contains` x then 0 else 1))

end TestAtLeast2
