package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait ImagingChannelInstances:
  given SimpleShow[ImagingChannel] =
    import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given_SimpleShow_:|
    summon[SimpleShow[Int :| Not[Negative]]].contramap(_.get)
