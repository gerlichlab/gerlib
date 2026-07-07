package at.ac.oeaw.imba.gerlich.gerlib.configuration
package instances

import cats.syntax.all.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader
import squants.space.Length

import at.ac.oeaw.imba.gerlich.gerlib.imaging.{Pixels3D, PixelDefinition}

/** PureConfig typeclasses instances for imaging-related domain-specific types */
trait PureConfigImagingInstances:
  given (ConfigReader[PixelDefinition]) => ConfigReader[Pixels3D] =
    deriveReader[Pixels3D]

  given (readLength: ConfigReader[Length]) => ConfigReader[PixelDefinition] =
    readLength.emap(l =>
      PixelDefinition
        .tryToDefine(l)
        .leftMap(msg =>
          CannotConvert(value = l.toString, toType = "PixelDefinition", because = msg)
        )
    )
end PureConfigImagingInstances
