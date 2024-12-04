package at.ac.oeaw.imba.gerlich.gerlib

import cats.syntax.all.*
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import at.ac.oeaw.imba.gerlich.gerlib.collections.{AtLeast2, lookupBySubset}

/** Tests for the {@code collections.lookupBySubset} function */
class TestLookupBySubset extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:
  test("When search pool is empty, result is always empty."):
    forAll: (query: Set[Int]) =>
      lookupBySubset(List())(query) shouldEqual None.asRight

  test("When search pool has exactly one entry, search never yields error (multi-hit) result."):
    forAll: (singlePoolEntry: (Set[Int], String), query: Set[Int]) =>
      lookupBySubset(List(singlePoolEntry))(query) match
      case Right(_) => succeed
      case Left(multiHit) =>
        fail(s"Single-entry search pool yielded multi-hit result for query $query: $multiHit")

  test(
    "Empty query yields empty result when pool is empty, good result when pool size is one, error result when pool is multiple."
  ):
    forAll: (searchPool: List[(Set[Int], String)]) =>
      val expectation: Either[AtLeast2[List, (Set[Int], String)], Option[String]] =
        searchPool match
        case Nil                      => None.asRight
        case (_, v) :: Nil            => v.some.asRight
        case entry1 :: entry2 :: rest => AtLeast2.listOf(entry1, entry2, rest).asLeft
      val observation = lookupBySubset(searchPool)(Set())
      observation shouldEqual expectation

  test(
    "When search pool members form a nested hierarchy, only the biggest entry can be the result of a unique hit."
  ):
    val namedBooleanPowerset: List[(Set[Boolean], String)] = List(
      Set(),
      Set(false),
      Set(true),
      Set(false, true)
    ).fproduct(_.map(_.toString).mkString)

    forAll: (query: Set[Boolean]) =>
      lookupBySubset(namedBooleanPowerset)(query) match {
      case Left(hits)       => query =!= Set(false, true) shouldBe true
      case Right(None)      => fail(s"Empty result for query: $query")
      case Right(Some(hit)) => hit shouldEqual "falsetrue"
      }
end TestLookupBySubset
