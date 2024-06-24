package at.ac.oeaw.imba.gerlich.gerlib.geometry

import cats.Contravariant

/**
 * Typeclass indicating that values of a particular type admit a 3D localisation
 * 
 * @tparam A The type of value which admits localisation by virtue of presence of instance of this typeclass
 */
trait Locatable[A, C]:
    /** Get the 3D localisation of a value of {@code A}. */
    def getPoint3D: A => Point3D[C]
end Locatable

/** Helpers for working with the [[at.ac.oeaw.imba.gerlich.gerlib.geometry.Locatable]] typeclass */
object Locatable:
    private type FixedCoordinateTypeLocatable[C] = [A] =>> Locatable[A, C]

    /** Maintain cats tradition of invariant typeclasses, but as with [[cats.Show]], e.g., provide contravariance mechanism. */
    given contravariantForLocatable[C]: Contravariant[FixedCoordinateTypeLocatable[C]] with
        /** Simply convert the B to an A, and then apply the localisation function for A */
        def contramap[A, B](fa: Locatable[A, C])(f: B => A): Locatable[B, C] = new:
            override def getPoint3D: B => Point3D[C] = f `andThen` fa.getPoint3D
end Locatable
