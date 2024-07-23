package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.derived.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.{Not, StrictEqual}
import io.github.iltotore.iron.constraint.char.{Digit, Letter}
import io.github.iltotore.iron.constraint.collection.{Empty, ForAll}

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given

/** Tools and types related to imaging */
package object imaging:
  /** A field of view is a value of one of a relatively small set of types. */
  type FieldOfViewLike = PositionName | FieldOfView

  /** Typeclass for which an instance provides a way to get a FOV-like value
    * from a value of another type
    */
  trait AdmitsFieldOfView[A, B <: FieldOfViewLike]:
    def getFieldOfView: A => B

  /** Type wrapper around 0-based index of field of view (FOV) */
  final case class FieldOfView(private[imaging] get: NonnegativeInt)
      derives Order

  /** Helpers for working with fields of view */
  object FieldOfView:
    /** Wrap the given value as a field of view, if it's valid as one. */
    def parse: String => Either[String, FieldOfView] =
      parseThroughNonnegativeInt("FieldOfView")(FieldOfView.apply)
  end FieldOfView

  /** A position name character must be alphanumeric, a hyphen, or an
    * underscore.
    */
  private type ValidPositionNameCharacter = Digit | Letter | StrictEqual["_"] |
    StrictEqual["-"]

  /** A position name must be nonempty and contain only certain characters. */
  private type PositionNameConstraint = Not[Empty] &
    ForAll[ValidPositionNameCharacter]

  /** The name of a position / field of view is a string whose characters all
    * fulfill the constraint.
    */
  type PositionName = String :| PositionNameConstraint

end imaging
