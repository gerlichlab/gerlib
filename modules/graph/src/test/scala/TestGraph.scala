package at.ac.oeaw.imba.gerlich.gerlib.graph

import scala.reflect.ClassTag
import scalax.collection.edges.UnDiEdge
import scalax.collection.generator.*
import scalax.collection.immutable.Graph

import cats.*
import cats.syntax.all.*
import cats.laws.discipline.MonoidKTests
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Test the functionality of the graph module. */
class TestGraph
    extends AnyFunSuiteLike,
      FunSuiteDiscipline,
      ScalaCheckPropertyChecks,
      should.Matchers:

  // needed since we're in AnyFunSuiteLike land
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val maxOrder: Int = 10

  private type NodeValue = Int

  // Generate this to avoid generator failing due to too many node duplicates.
  private type AdjMap = Map[NodeValue, Set[NodeValue]]

  def genRandomGraphMetrics[N: Arbitrary]: Gen[GraphGen.Metrics[N]] =
    Gen.choose(1, maxOrder).map { n => // RandomGraph throws exception for order-0.
      new GraphGen.Metrics[N]:
        override def order: Int = n
        override def nodeDegrees: NodeDegreeRange =
          // Don't let graph order approach vertex degreer, else
          // too many edge add tries will fail and the generator will stop.
          NodeDegreeRange(0, math.ceil(n / 3.0).toInt)
        override def nodeGen: Gen[N] = Arbitrary.arbitrary[N]
        override def connected: Boolean = false
    }

  given arbitrarySimplestGraph[N: Arbitrary: ClassTag]: Arbitrary[SimplestGraph[N]] =
    def genEmpty: Gen[SimplestGraph[N]] = Graph.empty
    def genNonEmpty: Gen[SimplestGraph[N]] =
      genRandomGraphMetrics.flatMap(
        GraphGen.fromMetrics[N, UnDiEdge[N], Graph](Graph, _, Set(UnDiEdge)).apply
      )
    Arbitrary { Gen.frequency(1 -> genEmpty, (maxOrder - 1) -> genNonEmpty) }

  given eqSimplestGraphByOuter[N: Eq]: Eq[SimplestGraph[N]] =
    Eq.by: g =>
      (g.nodes.toOuter, g.edges.toOuter)

  // Check laws for graphs monoid defined by outer-node union and outer-edge union.
  checkAll(
    "graph.SimplestGraph.MonoidKLaws",
    MonoidKTests[SimplestGraph](using monoidKForSimplestGraphByOuterElements).monoidK[Int]
  )

  test("graph.buildSimpleGraph yields a graph with correct node set."):
    forAll: (adjMap: AdjMap) =>
      val adjList = adjMap.toList
      val expNodes = adjList.map(_._1).toSet | adjList.map(_._2).combineAll
      val obsNodes =
        val g = buildSimpleGraph(adjList)
        g.nodes.toOuter
      obsNodes shouldEqual expNodes

  test(
    "After graph.buildSimpleGraph, Every edge connects one key and one value from the adjacency list."
  ):
    forAll: (adjMap: AdjMap) =>
      val adjList = adjMap.toList

      // Take each (src, dst) pair from the graph's "outer" edges.
      val obsEndpoints: Set[(NodeValue, NodeValue)] =
        buildSimpleGraph(adjList).edges.toOuter
          .map: e =>
            (e.source -> e.target)

      // Each pair in the adjacency list input should yield pairs of edge
      // endpoints, but since the edges of the graph are undirected, then
      // regard the symmetric (src, dst) and (dst, src) as equivalent
      // when building up the collection of expected endpoints.
      val expEndpoints: List[(NodeValue, NodeValue)] =
        adjList
          .foldLeft(List.empty[(NodeValue, NodeValue)] -> Set.empty[(NodeValue, NodeValue)]) {
            case ((acc, seen), (k, vs)) =>
              val additions = vs.map(k -> _).filterNot { (src, dst) =>
                seen.contains(src -> dst) || seen.contains(dst -> src)
              }
              (additions.toList ::: acc, seen ++ additions)
          }
          ._1

      val (expDisordered, obsDisordered) =
        def disorderEndpoints =
          val f = (a: NodeValue, b: NodeValue) => Set(a, b)
          f.tupled
        (expEndpoints.map(disorderEndpoints).toSet, obsEndpoints.map(disorderEndpoints))

      // There should be no endpoint pairs which are equivalent up to permutation.
      expDisordered.size shouldEqual expEndpoints.length
      obsDisordered.size shouldEqual obsEndpoints.size

      // The number of endpoints in our expected collection should match
      // the number observed, since we took care of symmetric endpoints
      // equivalence in the construction of the expectation.
      expEndpoints.length shouldEqual obsEndpoints.size

      // The actual identities of the endpoints should match, since in the
      // construction of the expected collection, we added elements in the
      // order in which they appear in the adjacency list.
      val extraExp = expDisordered -- obsDisordered
      val extraObs = obsDisordered -- expDisordered
      if extraExp.isEmpty && extraObs.isEmpty then succeed
      else fail(s"Extra expected: $extraExp. Extra observed: $extraObs")

end TestGraph
