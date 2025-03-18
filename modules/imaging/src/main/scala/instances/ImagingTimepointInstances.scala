package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.json.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given

/** Typeclass instances for working with imaging timepoint values */
trait ImagingTimepointInstances:
  /** Simply show a timepoint by its underlying integer value. */
  given SimpleShow[ImagingTimepoint] =
    summon[SimpleShow[NonnegativeInt]].contramap(_.get)

  given JsonValueWriter[ImagingTimepoint, ujson.Num]:
    override def apply(t: ImagingTimepoint): ujson.Num = ujson.Num(t.get)
