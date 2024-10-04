package at.ac.oeaw.imba.gerlich.gerlib.testing
package syntax

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

/** Syntax enrichment on ScalaCheck data types */
trait SyntaxForScalacheck:

  /** Add nicer syntax to [[org.scalacheck.Arbitrary]] instances. */
  extension [A](arb: Arbitrary[A])
    infix def suchThat(p: A => Boolean): Arbitrary[A] =
      (arb.arbitrary `suchThat` p).toArbitrary

  /** Add nicer syntax to the [[org.scalacheck.Arbitrary]] companion object. */
  extension (Arb: Arbitrary.type)
    def oneOf[A: Arbitrary, B: Arbitrary]: Arbitrary[A | B] =
      Arbitrary { Gen.oneOf(arbitrary[A], arbitrary[B]) }

  /** Add nicer syntax to generators. */
  extension [A](g: Gen[A])
    def toArbitrary: Arbitrary[A] = Arbitrary(g)
    infix def zipWith[B](b: B): Gen[(A, B)] = g.map(_ -> b)
end SyntaxForScalacheck
