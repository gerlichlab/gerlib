package at.ac.oeaw.imba.gerlich.gerlib

/** Aggregation of syntax enrichments, for convenenience as imports */
package object syntax:
  /** Aggregation of syntax enrichments, for convenenience as imports */
  object all extends AllSyntax

  trait AllSyntax extends SyntaxForOption, SyntaxForSimpleShow, SyntaxForTuple2
