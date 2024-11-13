package at.ac.oeaw.imba.gerlich.gerlib

import cats.data.*
import cats.laws.discipline.arbitrary.given // for Arbitrary[NonEmptySet[*]], Arbitrary[NonEmptyList[*]], etc.
import cats.laws.discipline.SemigroupKTests
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import cats.syntax.all.*
import io.github.iltotore.iron.Constraint
import io.github.iltotore.iron.constraint.collection.MinLength
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.collections.{AtLeast2, AtLeast2List, AtLeast2Set}

/** Tests for the [[collections.AtLeast2]] refinement type
  */
class TestAtLeast2
    extends AnyFunSuiteLike,
      FunSuiteDiscipline,
      ScalaCheckPropertyChecks,
      should.Matchers:

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500)

  inline given arbitraryForAtLeast2[C[*] <: Iterable[*], E](using
      Arbitrary[E],
      org.scalacheck.util.Buildable[E, C[E]],
      C[E] => Iterable[E],
      Constraint[C[E], MinLength[2]]
  ): Arbitrary[AtLeast2[C, E]] = Arbitrary {
    Gen.size
      .flatMap: k =>
        Gen.choose(2, scala.math.max(2, k))
      .flatMap(Gen.containerOfN[C, E](_, Arbitrary.arbitrary[E]))
      .suchThat(_.size > 2)
      .map { unrefined =>
        AtLeast2
          .either(unrefined)
          .fold(
            msg =>
              throw new Exception(
                s"Error generating collection of at least 2 elements! Collection: ${unrefined}. Message: $msg"
              ),
            identity
          )
      }
  }

  test("For lists, AtLeast2.apply has cons-like argument order"):
    assertTypeError:
      "AtLeast2(NonEmptyList.one(1), 0)"
    assertCompiles:
      "AtLeast2(0, NonEmptyList.one(1))"

  test("For lists, AtLeat2 is correct with apply-syntax"):
    import at.ac.oeaw.imba.gerlich.gerlib.collections.AtLeast2.syntax.*
    forAll: (xs: NonEmptyList[Int], x: Int) =>
      val atLeast2 = AtLeast2(x, xs)
      atLeast2 `contains` x shouldBe true
      atLeast2.size shouldEqual 1 + xs.size

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

  // Check the SemigroupK laws for the at-least-2-element refinement of List.
  checkAll("AtLeast2[List, *].SemigroupKLaws", SemigroupKTests[AtLeast2List].semigroupK[Int])

  // Check the SemigroupK laws for the at-least-2-element refinement of Set.
  checkAll("AtLeast2[Set, *].SemigroupKLaws", SemigroupKTests[AtLeast2Set].semigroupK[Int])

  test("When syntax is imported, AtLeast2[Set, A] may be represented as cats.data.NonEmptySet[A]."):
    assertCompiles("AtLeast2.unsafe(Set(1, 2))") // Precondition: we can build the collection.
    assertTypeError(
      "AtLeast2.unsafe(Set(1, 2)).toNes"
    ) // Test: without syntax import, .toNes is unavailable.
    assertCompiles(
      "import AtLeast2.syntax.toNes; AtLeast2.unsafe(Set(1, 2)).toNes"
    ) // Test: with syntax import, .toNes becomes available.

    // Check the equivalence of the result of the .toNes operation and the underlying collection.
    import AtLeast2.syntax.{toNes, toSortedSet}
    forAll: (xs: AtLeast2[Set, Int]) =>
      xs.toNes shouldEqual NonEmptySet.fromSetUnsafe(xs.toSortedSet)

  test("When syntax is imported, AtLeast2[Set, A] may be represented as simply Set[A]."):
    assertCompiles("AtLeast2.unsafe(Set(1, 2))") // Precondition: we can build the collection.
    assertTypeError(
      "AtLeast2.unsafe(Set(1, 2)).toSet"
    ) // Test: without syntax import, .toNes is unavailable.
    assertCompiles(
      "import AtLeast2.syntax.toSet; AtLeast2.unsafe(Set(1, 2)).toSet"
    ) // Test: with syntax import, .toNes becomes available.

end TestAtLeast2
