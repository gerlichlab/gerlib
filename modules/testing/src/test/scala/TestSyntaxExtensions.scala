package at.ac.oeaw.imba.gerlich.gerlib
package testing

import cats.Alternative
import cats.syntax.all.*

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.testing.syntax.SyntaxForScalacheck

/** Tests for for the syntax extensions we provide to Scalacheck instances and
  * companion objects
  */
class TestSyntaxExtensions
    extends AnyFunSuite,
      ScalaCheckPropertyChecks,
      should.Matchers,
      SyntaxForScalacheck // the trait under test
      :
  test(
    "Arbitrary.oneOf produces values of each type, in plausible relative proportions."
  ) {
    /* Establish the interval for plausible counts based on Normal approximation to the Binomial. */
    val n = 10000
    val (lowerBound, upperBound) =
      val zStar = 3.719016
      val p = 0.5
      val sd = scala.math.sqrt(n * p * p)
      val exp = n * p
      (-zStar * sd + exp, zStar * sd + exp)

    // the instance under test
    given Arbitrary[Boolean | Int] = Arbitrary.oneOf[Boolean, Int]

    // Use the instance under test to generate the values, then count by type.
    forAll(Gen.listOfN(n, arbitrary[Boolean | Int])) { values =>
      val (bools, ints): (List[Boolean], List[Int]) =
        Alternative[List].separate(
          values map {
            case p: Boolean => p.asLeft
            case z: Int     => z.asRight
          }
        )
      bools.length > lowerBound shouldBe true
      bools.length < upperBound shouldBe true
      ints.length > lowerBound shouldBe true
      ints.length < upperBound shouldBe true
    }
  }
end TestSyntaxExtensions
