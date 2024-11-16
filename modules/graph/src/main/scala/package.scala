package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import scalax.collection.edges.UnDiEdge
import scalax.collection.edges.UnDiEdgeImplicits // for syntax, like ~
import scalax.collection.immutable

/** Graph-related operations */
package object graph:
  type SimplestGraph[N] = immutable.Graph[N, UnDiEdge[N]]

  /** Construct a simple graph from an adjancency list/'matrix'.
    *
    * @tparam N
    *   The node type
    * @param adj
    *   The adjacency list / 'matrix' which encodes the edge relationships / node adjacencies
    */
  def buildSimpleGraph[N](adj: List[(N, Set[N])]): SimplestGraph[N] =
    val singleGraphs = adj.map(fromSingleNodeAndNeighbors[N].tupled)
    monoidKForSimplestGraphByOuterElements.combineAllK(singleGraphs)

  /** Construct a simple graph from a set of nodes and set of edge endpoints.
    *
    * @tparam N
    *   The node type
    * @param adj
    *   The endpoints of edges to build
    */
  def buildSimpleGraph[N](nodes: Set[N], endpoints: Set[(N, N)]): SimplestGraph[N] =
    immutable.Graph.from(nodes, endpoints.map(_ ~ _))

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
end graph
