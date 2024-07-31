package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.derived.*
import cats.syntax.all.*

import io.github.iltotore.iron.{:|, refineEither}
import io.github.iltotore.iron.constraint.any.{Not, StrictEqual}
import io.github.iltotore.iron.constraint.char.{Digit, Letter}
import io.github.iltotore.iron.constraint.collection.{Empty, ForAll}

import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.instances.nonnegativeInt.given

/** Tools and types related to imaging */
package object imaging:
  private type Parser[Result] = String => Either[String, Result]

  /** A field of view is a value of one of a relatively small set of types. */
  sealed trait FieldOfViewLike

  /** Helpers for working with FOV-like values */
  object FieldOfViewLike:
    def parse: Parser[FieldOfViewLike] =
      s =>
        FieldOfView.parse(s).leftFlatMap { e1 =>
          PositionName.parse(s).leftMap { e2 =>
            s"Could not parse FOV-like. Message 1: $e1. Message 2: $e2"
          }
        }

  /** Type wrapper around 0-based index of field of view (FOV) */
  final case class FieldOfView(private[imaging] get: NonnegativeInt)
      extends FieldOfViewLike derives Order

  /** Helpers for working with fields of view */
  object FieldOfView:
    /** Wrap the given value as a field of view, if it's valid as one. */
    def parse: Parser[FieldOfView] =
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
  final case class PositionName(
      private[imaging] get: String :| PositionNameConstraint
  ) extends FieldOfViewLike

  /** Put instances here since the refinement is opaque, so underlying String
    * won't be visible elsewhere.
    */
  object PositionName:
    /** Refine through [[scala.util.Either]] as the monadic type. */
    def parse: Parser[PositionName] =
      _.refineEither[PositionNameConstraint].map(PositionName.apply)

    /** Ordering is by natural (lexicographical) text ordering */
    given Order[PositionName] = Order.by(_.get: String)

    /** Show the value as its simple text representation. */
    given Show[PositionName] = Show.show(_.get: String)

    /** Show the value as its simple text representation. */
    given SimpleShow[PositionName] = SimpleShow.instance(_.get: String)
  end PositionName

end imaging
