package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.json.*

/** Typeclass instances for working with imaging timepoint values */
trait ImagingTimepointInstances:
  /** Simply show a timepoint by its underlying integer value. */
  given SimpleShow[ImagingTimepoint] =
    import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given_SimpleShow_:|
    summon[SimpleShow[Int :| Not[Negative]]].contramap(_.get)

  given JsonValueWriter[ImagingTimepoint, ujson.Num]:
    override def apply(t: ImagingTimepoint): ujson.Num = ujson.Num(t.get)
