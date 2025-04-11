package at.ac.oeaw.imba.gerlich.gerlib.numeric

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for positive integer refinement type */
class TestNonnegativeInt extends AnyFunSuite, should.Matchers, ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  test(
    "NonnegativeInt correctly restricts which expressions compile by constructor syntax."
  ) {
    // NB: Here, autoRefine isn't needed; it's provided by apply() in our subtrait of RefinedTypeOps.
    assertCompiles("NonnegativeInt(1)")
    assertCompiles("NonnegativeInt(0)")
    assertTypeError("NonnegativeInt(-2)")
  }

  test("NonnegativeInt.option behaves correctly.") {
    forAll { (z: Int) =>
      NonnegativeInt.option(z) match
      case None if z < 0     => succeed
      case Some(n) if z >= 0 => z shouldEqual n
      case bad               => fail(s"NonnegativeInt.maybe($z) gave bad result: $bad")
    }
  }

  test("The SimpleShow instance for nonnegative integer just shows the number."):
    import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*
    import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given_SimpleShow_:|
    import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

    given Arbitrary[Int :| Not[Negative]] = Arbitrary:
      Gen
        .choose(0, Int.MaxValue)
        .map(z =>
          NonnegativeInt.option(z).getOrElse {
            throw IllegalRefinement(z, "Can't refine as nonnegative")
          }
        )

    forAll { (n: Int :| Not[Negative]) => n.show_ shouldEqual n.toString }
end TestNonnegativeInt
