package at.ac.oeaw.imba.gerlich.gerlib.graph

import scala.reflect.ClassTag
import scalax.collection.edges.UnDiEdge
import scalax.collection.generator.*
import scalax.collection.immutable.Graph

import cats.*
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

  private val maxOrder: Int = 20

  given arbitraryForRandomGraphMetrics[N: Arbitrary]: Arbitrary[GraphGen.Metrics[N]] =
    Arbitrary {
      Gen.choose(1, maxOrder).map { n => // RandomGraph throws exception for order-0.
        new GraphGen.Metrics[N]:
          override def order: Int = n
          override def nodeDegrees: NodeDegreeRange =
            // Don't let graph order approach vertex degreer, else
            // too many edge add tries will fail and the generator will stop.
            NodeDegreeRange(0, n / 2)
          override def nodeGen: Gen[N] = Arbitrary.arbitrary[N]
          override def connected: Boolean = false
      }
    }

  given arbitrarySimplestGraph[N: Arbitrary: ClassTag]: Arbitrary[SimplestGraph[N]] =
    def genEmpty: Gen[SimplestGraph[N]] = Graph.empty
    def genNonEmpty: Gen[SimplestGraph[N]] = Arbitrary
      .arbitrary[GraphGen.Metrics[N]]
      .flatMap { metrics =>
        GraphGen.fromMetrics[N, UnDiEdge[N], Graph](Graph, metrics, Set(UnDiEdge)).apply
      }
    Arbitrary { Gen.frequency(1 -> genEmpty, (maxOrder - 1) -> genNonEmpty) }

  given eqSimplestGraphByOuter[N: Eq]: Eq[SimplestGraph[N]] =
    Eq.by: g =>
      (g.nodes.toOuter, g.edges.toOuter)

  // needed since we're in AnyFunSuiteLike land
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  // Check the SemigroupK laws for the at-least-2-element refinement of Set.
  checkAll(
    "graph.SimplestGraph.MonoidKLaws",
    MonoidKTests[SimplestGraph](using monoidKForSimplestGraphByOuterElements).monoidK[Int]
  )
end TestGraph
