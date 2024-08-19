package at.ac.oeaw.imba.gerlich.gerlib.testing

/** Syntax enrichment on values of testing-related data types */
package object syntax:
  object all extends AllSyntax

  trait AllSyntax extends SyntaxForScalacheck
end syntax
