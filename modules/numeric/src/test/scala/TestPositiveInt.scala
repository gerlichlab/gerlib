package at.ac.oeaw.imba.gerlich.gerlib.numeric

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for positive integer refinement type */
class TestPositiveInt extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  test(
    "PositiveInt correctly restricts which expressions compile by constructor syntax."
  ) {
    // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
    assertCompiles("PositiveInt(1)")
    assertTypeError("PositiveInt(0)")
    assertTypeError("PositiveInt(-2)")
  }

  test("PositiveInt.option behaves correctly.") {
    forAll { (z: Int) =>
      PositiveInt.option(z) match
      case None if z <= 0   => succeed
      case Some(n) if z > 0 => z shouldEqual n
      case bad              => fail(s"PositiveInt.maybe($z) gave bad result: $bad")
    }
  }
end TestPositiveInt
