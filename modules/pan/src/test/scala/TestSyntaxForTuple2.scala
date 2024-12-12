package at.ac.oeaw.imba.gerlich.gerlib

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for the syntax enrichment on Tuple2 */
class TestSyntaxForTuple2 extends AnyFunSuite, ScalaCheckPropertyChecks, should.Matchers:
  import at.ac.oeaw.imba.gerlich.gerlib.syntax.tuple2.*

  test(".swapBy and .flipBy are equivalent."):
    type Item = (String, Int)
    forAll: (ab: (Item, Item)) =>
      ab.swapBy(_._2) shouldEqual ab.flipBy(_._2)

  test(".flipBy is idempotent."):
    type Item = (Int, String)

    val key: Item => Int = _._1

    @annotation.tailrec
    def doFlips(t: (Item, Item), n: Int): (Item, Item) =
      import cats.syntax.all.*
      if n === 0 then t
      else doFlips(t.flipBy(key), n - 1)

    forAll(arbitrary[(Item, Item)], Gen.choose(2, 10)): (ab, n) =>
      doFlips(ab, n) shouldEqual ab.flipBy(key)

  test("After .flipBy, the first element's key is never greater than the second's key."):
    type Item = (Char, Int)

    val key: Item => Int = _._2

    forAll: (ab: (Item, Item)) =>
      ab.flipBy(key) match { case (a, b) => key(a) > key(b) shouldBe false }
end TestSyntaxForTuple2
