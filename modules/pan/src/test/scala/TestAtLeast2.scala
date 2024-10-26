package at.ac.oeaw.imba.gerlich.gerlib

import cats.data.*
import cats.laws.discipline.arbitrary.given // for Arbitrary[NonEmptySet[*]], Arbitrary[NonEmptyList[*]], etc.
import io.github.iltotore.iron.Constraint
import io.github.iltotore.iron.constraint.collection.MinLength
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2

/** Tests for the [[collections.AtLeast2]] refinement type
  */
class TestAtLeast2 extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:

  inline given arbitraryForAtLeast2[C[*], E](using
      Arbitrary[E],
      org.scalacheck.util.Buildable[E, C[E]],
      C[E] => Iterable[E],
      Constraint[C[E], MinLength[2]]
  ): Arbitrary[AtLeast2[C, E]] = Arbitrary {
    for
      n <- Gen.choose(2, 5)
      unrefined <- Gen.containerOfN[C, E](n, Arbitrary.arbitrary[E])
    yield AtLeast2
      .either(unrefined)
      .fold(
        msg =>
          throw new Exception(
            s"Error generating collection of at least 2 elements! Collection: ${unrefined}. Message: $msg"
          ),
        identity
      )
  }

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
    import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2.syntax.*
    forAll: (xs: NonEmptyList[Int], x: Int) =>
      val atLeast2 = AtLeast2(x, xs)
      atLeast2 `contains` x shouldBe true
      atLeast2.size shouldEqual 1 + xs.size

  test("For sets, AtLeast2 is correct with apply-syntax."):
    import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2.syntax.*
    forAll: (xs: NonEmptySet[Int], x: Int) =>
      // The test's principle is invalid if the "extra" element is already in the collection.
      whenever(!xs.contains(x)):
        val atLeast2 = AtLeast2(xs, x)
        atLeast2 `contains` x shouldBe true
        atLeast2.size shouldEqual (xs.length + (if xs `contains` x then 0 else 1))

  test(
    ".map on a AtLeast2[C, *] value returns a refined value IF AND ONLY IF a functor is available for the underlying container type and the AtLeast2 syntax is imported"
  ):
    assertCompiles("summon[cats.Functor[List]]") // Functor is available for List.
    assertTypeError("summon[cats.Functor[Set]]") // No functor for Set
    assertCompiles(
      "AtLeast2.unsafe(List(0, 1, 2))"
    ) // necessary precondition to render subsequent checks meaningful
    assertTypeError("AtLeast2.unsafe(List(0, 1, 2)).map(_ + 1)") // Haven't imported the .map syntax
    assertCompiles(
      "import AtLeast2.syntax.map; AtLeast2.unsafe(List(0, 1, 2)).map(_ + 1): AtLeast2[List, Int]"
    ) // With the proper syntax import, the .map operation works.

  test("Wrapping a list in the AtLeast2 refinement doesn't change the .head element."):
    import AtLeast2.syntax.head
    forAll: (x: Int, xs: NonEmptyList[Int]) =>
      AtLeast2(x, xs).head shouldEqual x

end TestAtLeast2
