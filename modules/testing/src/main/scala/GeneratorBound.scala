package at.ac.oeaw.imba.gerlich.gerlib.testing

import at.ac.oeaw.imba.gerlich.gerlib.numeric.extrema.*

/** Bounding the values of a particular type to be generated */
sealed trait GeneratorBound[A]:
    /** The bound (lower or upper) on values of {@code A} to generate */
    def value: A

/** Bounding the values of a particular type to be generated */
object GeneratorBound:
    /** Lower bound on values to generate of the given type. */
    final case class Lower[A](value: A) extends GeneratorBound[A]

    /** Helpers for working with lower bounds */
    object Lower:
        import MinimumValue.*  // for the syntax on a value of type MinimumValue[A]
        
        /** Wrap the given type's minimum value as a lower bound. */
        def minimum[A](using ev: AdmitsMinimum[A]): Lower[A] = Lower(ev.minimum.asBaseTypeValue)
        
        /** Alias for [[Lower.minimum]] */
        def trivial[A : AdmitsMinimum]: Lower[A] = minimum[A]
    
    /** Upper bound on values to generate of the given type. */
    final case class Upper[A](value: A) extends GeneratorBound[A]

    /** Helpers for working with upper bounds */
    object Upper:
        import MaximumValue.*  // for the syntax on a value of type MaximumValue[A]
        
        /** Wrap the given type's maximum value as an upper bound. */
        def maximum[A](using ev: AdmitsMaximum[A]): Upper[A] = Upper(ev.maximum.asBaseTypeValue)
        
        /** Alias for [[Upper.maximum]] */
        def trivial[A : AdmitsMaximum]: Upper[A] = maximum[A]
end GeneratorBound
