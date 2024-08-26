package at.ac.oeaw.imba.gerlich.gerlib.cell

import cats.Order
import cats.syntax.all.*

import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import at.ac.oeaw.imba.gerlich.gerlib.numeric.PositiveInt
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.all.given

/** Tests for data type for nucleuar / non-nuclear attribution */
class TestNuclearDesignation
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      should.Matchers:
  given Arbitrary[PositiveInt] = Arbitrary {
    Gen.choose(1, Int.MaxValue).map(PositiveInt.unsafe)
  }

  given arbitraryForNucNum(using
      Arbitrary[PositiveInt]
  ): Arbitrary[NucleusNumber] =
    Arbitrary { Arbitrary.arbitrary[PositiveInt].map(NucleusNumber.apply) }

  given arbitraryForNuclearDesignation(using
      Arbitrary[NucleusNumber]
  ): Arbitrary[NuclearDesignation] =
    Arbitrary {
      Gen.oneOf(Arbitrary.arbitrary[NucleusNumber], Gen.const(OutsideNucleus))
    }

  test(
    "Order[NuclearDesignation]: Whenever the first compared element is non-nuclear, it comes first."
  ) {
    given Order[NuclearDesignation] = getNonNucFirstOrder

    forAll { (b: NuclearDesignation) =>
      val a: NuclearDesignation = OutsideNucleus
      a < b shouldBe true
    }
  }

  test(
    "Order[NuclearDesignation]: Whenever the first element is nuclear and the second isn't, the second one comes first."
  ) {
    given Order[NuclearDesignation] = getNonNucFirstOrder

    forAll { (nucNum: NucleusNumber) =>
      val a: NuclearDesignation = nucNum
      val b: NuclearDesignation = OutsideNucleus
      a > b shouldBe true
    }
  }

  test(
    "Order[NuclearDesignation]: whenver both compared values are numbers, the comparison on the wrapped values."
  ) {
    given Order[NuclearDesignation] = getNonNucFirstOrder

    forAll { (a: NucleusNumber, b: NucleusNumber) =>
      val byNucDes = Order[NuclearDesignation].compare(a, b).sign
      val byWrapped = Order[PositiveInt].compare(a.get, b.get).sign
      byNucDes shouldEqual byWrapped
    }
  }

  /** Convenience helper to fetch a particular ordering of nuclear designation
    * values
    */
  def getNonNucFirstOrder: Order[NuclearDesignation] =
    NuclearDesignation.orderWithNonNuclearFirst
end TestNuclearDesignation
