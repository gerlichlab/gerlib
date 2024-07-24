package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.derived.*

import io.github.iltotore.iron.{ :|, refineEither }
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
  opaque type PositionName = String :| PositionNameConstraint

  /** Put instances here since the refinement is opaque, so underlying String won't be visible elsewhere. */
  object PositionName:
    /** Refine through [[scala.util.Either]] as the monadic type. */
    def parse: String => Either[String, PositionName] = _.refineEither

    /** Ordering is by natural (lexicographical) text ordering */
    given Order[PositionName] = Order.by{ s => s: String }

    /** Show the value as its simple text representation. */
    given Show[PositionName] = Show.show{ s => s: String }

    /** Show the value as its simple text representation. */
    given SimpleShow[PositionName] = SimpleShow.instance{ s => s: String }
  end PositionName

end imaging
