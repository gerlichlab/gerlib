package at.ac.oeaw.imba.gerlich.gerlib.geometry

import cats.Contravariant

/** Typeclass indicating that values of a particular type admit a 3D
  * localisation
  *
  * @tparam A
  *   The type of value which admits localisation by virtue of presence of
  *   instance of this typeclass
  * @tparam C
  *   The type of value wrapped as coordinates for the point
  */
trait Locatable[A, C]:
  /** Get the 3D localisation of a value of {@code A}. */
  def getPoint3D: A => Point3D[C]
end Locatable

/** Helpers for working with the
  * [[at.ac.oeaw.imba.gerlich.gerlib.geometry.Locatable]] typeclass
  */
object Locatable:
  /** Maintain cats tradition of invariant typeclasses, but as with
    * [[cats.Show]] e.g., provide contravariance mechanism.
    *
    * This instance says that if we know how to get--from any `A`--a 3D point
    * with coordinates wrapping values of type `C`, and we're told how to get an
    * `A` from a `B`, then we also know how to get a 3D point wrapping
    * coordinates of type `C` from any `B`.
    */
  given contravariantForLocatable[C]: Contravariant[[A] =>> Locatable[A, C]]
  with
    /** Simply convert the B to an A, and then apply the localisation function
      * for A.
      */
    def contramap[A, B](fa: Locatable[A, C])(f: B => A): Locatable[B, C] = new:
      override def getPoint3D: B => Point3D[C] = f `andThen` fa.getPoint3D
end Locatable
