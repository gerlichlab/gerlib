package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import scalax.collection.edges.UnDiEdge
import scalax.collection.edges.UnDiEdgeImplicits // for syntax, like ~
import scalax.collection.immutable

/** Graph-related operations */
package object graph:
  private[graph] type SimplestGraph[N] = immutable.Graph[N, UnDiEdge[N]]

  /** Start with an empty graph, and combine by taking union of node and edge sets. */
  def monoidKForSimplestGraphByOuterElements: MonoidK[SimplestGraph] = new:
    override def empty[N]: SimplestGraph[N] = immutable.Graph.empty
    override def combineK[N](g1: SimplestGraph[N], g2: SimplestGraph[N]): SimplestGraph[N] =
      immutable.Graph.from(
        (g1.nodes.toOuter | g2.nodes.toOuter),
        g1.edges.toOuter | g2.edges.toOuter
      )

  private def fromSingleNodeAndNeighbors[N](n: N, neighbors: Set[N]): SimplestGraph[N] =
    immutable.Graph.from(Set(n), neighbors.map(n ~ _))

  /** Construct a simple graph from an adjancency list/'matrix'.
    *
    * @tparam N
    *   The node type
    * @param adjecency
    *   The adjacency list / 'matrix' which encodes the edge relationships / node adjacencies
    */
  def buildSimpleGraph[N](adjacency: Map[N, Set[N]]): SimplestGraph[N] =
    val singleGraphs = adjacency.toList.map(fromSingleNodeAndNeighbors[N].tupled)
    monoidKForSimplestGraphByOuterElements.combineAllK(singleGraphs)
end graph
