package at.ac.oeaw.imba.gerlich.gerlib.configuration
package instances

import cats.syntax.all.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import squants.space.Length

/** PureConfig typeclass instances for squants */
trait PureConfigSquantsInstances:
  given (readString: ConfigReader[String]) => ConfigReader[Length] =
    readString.emap: s =>
      Length
        .parseString(s)
        .toEither
        .leftMap { e =>
          CannotConvert(
            value = s,
            toType = "squants.space.Length",
            because = e.getMessage
          )
        }
end PureConfigSquantsInstances
