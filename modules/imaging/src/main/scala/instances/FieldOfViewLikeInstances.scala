package at.ac.oeaw.imba.gerlich.gerlib.imaging
package instances

import cats.syntax.all.*
import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.numeric.Negative

import at.ac.oeaw.imba.gerlich.gerlib.SimpleShow
import at.ac.oeaw.imba.gerlich.gerlib.json.JsonValueWriter
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given
import at.ac.oeaw.imba.gerlich.gerlib.syntax.all.*

/** Typeclass instances for types related to representation of imaging field of view
  */
trait FieldOfViewLikeInstances:
  given JsonValueWriter[PositionName, ujson.Str]:
    override def apply(posName: PositionName): ujson.Str =
      ujson.Str(posName.get)

  given JsonValueWriter[FieldOfView, ujson.Num]:
    override def apply(fov: FieldOfView): ujson.Num =
      ujson.Num(fov.get)

  /** Simply show a field of view by the text representation of the underlying integer value.
    */
  given SimpleShow[FieldOfView] =
    summon[SimpleShow[Int :| Not[Negative]]].contramap(_.get)

  /** Simply show a position name by the underlying value. */
  given SimpleShow[PositionName] = SimpleShow.instance(_.get)

  /** Simply show a general field-of-view-like by distinguishing among the subtypes and choosing the
    * appropriate instance.
    */
  given (SimpleShow[FieldOfView], SimpleShow[PositionName]) => SimpleShow[FieldOfViewLike] = new:
    override def show_(fovLike: FieldOfViewLike): String = fovLike match
    case fov: FieldOfView  => fov.show_
    case pos: PositionName => pos.show_
end FieldOfViewLikeInstances
