package at.ac.oeaw.imba.gerlich.gerlib.syntax

/** Syntax enrichment on [[scala.Option]] */
object option:
  /** Syntax for when a value wrapped as optional is provably nonempty */
  extension [A](sa: Some[A])
    /** Given that the optional value's provably nonempty, provide value
      * extraction helper.
      */
    def extractValue: A = sa match { case Some(a) => a }
end option
