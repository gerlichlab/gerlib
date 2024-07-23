package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow

trait PositionNameInstances:
  given SimpleShow[PositionName] = SimpleShow.fromToString
