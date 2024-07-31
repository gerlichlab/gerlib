package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for types related to representation of imaging field of
  * view
  */
trait FieldOfViewLikeInstances:
  /** Simply show a field of view by the text representation of the underlying
    * integer value.
    */
  given SimpleShow[FieldOfView] =
    summon[SimpleShow[NonnegativeInt]].contramap(_.get)

  /** Simply show a position name by the underlying value. */
  given SimpleShow[PositionName] = SimpleShow.fromToString

  /** Simply show a general field-of-view-like by distinguishing among the
    * subtypes and choosing the appropriate instance.
    */
  given simpleShowForFovLike(using
      showFov: SimpleShow[FieldOfView],
      showPos: SimpleShow[PositionName]
  ): SimpleShow[FieldOfViewLike] with
    override def show_(fovLike: FieldOfViewLike): String = fovLike match {
      case fov: FieldOfView  => fov.show_
      case pos: PositionName => pos.show_
    }
end FieldOfViewLikeInstances