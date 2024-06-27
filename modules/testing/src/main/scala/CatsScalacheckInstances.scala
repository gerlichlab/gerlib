package at.ac.oeaw.imba.gerlich.gerlib.testing

import cats.*
import org.scalacheck.{ Arbitrary, Gen }

/** [[cats]] instances for [[org.scalacheck]] data types */
trait CatsScalacheckInstances:
    given applicativeForArbitrary: Applicative[Arbitrary] with
        /** Define the pure operation by always generating the given value. */
        override def pure[A](x: A): Arbitrary[A] = Arbitrary{ Gen.const(x) }
        /** Define application by generating the arbitrary function, and then applying it to the arbitrary value. */
        override def ap[A, B](ff: Arbitrary[A => B])(fa: Arbitrary[A]): Arbitrary[B] = Arbitrary{ 
            for {
                f <- ff.arbitrary
                a <- fa.arbitrary
            } yield f(a)
        }
end CatsScalacheckInstances
