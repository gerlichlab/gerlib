package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt

trait ImagingChannelInstances:
  import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given
  given simpleShowForChannel(using
      ev: SimpleShow[NonnegativeInt]
  ): SimpleShow[ImagingChannel] =
    ev.contramap(_.get)
