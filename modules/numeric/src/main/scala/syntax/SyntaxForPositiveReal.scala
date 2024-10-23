package at.ac.oeaw.imba.gerlich.gerlib.numeric
package syntax

import io.github.iltotore.iron.refineUnsafe

trait SyntaxForPositiveReal:
  /** Enable the refinement of autoRefine in client code where the import's not present.
    */
  extension (x: PositiveReal) def asNonnegative: NonnegativeReal = x.refineUnsafe
