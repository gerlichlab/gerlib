package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt

trait ImagingChannelInstances:
  import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given
  given SimpleShow[ImagingChannel] =
    summon[SimpleShow[NonnegativeInt]].contramap(_.get)
