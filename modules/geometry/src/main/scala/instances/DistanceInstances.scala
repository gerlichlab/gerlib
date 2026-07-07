package at.ac.oeaw.imba.gerlich.gerlib.geometry
package instances

import cats.Show
import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

/** Typeclass instances related to distance values */
trait DistanceInstances:
  /** Show the quantity and units with a space in between.
    *
    * This format is chosen so that the rendered value roundtrips through `Distance.parse`, which
    * delegates to squants' `Length` parser.
    */
  given Show[Distance] = Show.show { d =>
    s"${d.value} ${d.unit.symbol}"
  }

  /** Use .show to derive .show_ */
  given Show[Distance] => SimpleShow[Distance] = SimpleShow.fromShow
end DistanceInstances
