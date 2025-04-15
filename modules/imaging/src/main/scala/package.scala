package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.derived.*
import cats.syntax.all.*

import io.github.iltotore.iron.{:|, refineEither}
import io.github.iltotore.iron.cats.given
import io.github.iltotore.iron.constraint.any.{Not, StrictEqual}
import io.github.iltotore.iron.constraint.char.{Digit, Letter}
import io.github.iltotore.iron.constraint.collection.{Empty, ForAll}
import io.github.iltotore.iron.constraint.numeric.{Negative, Positive}
import io.github.iltotore.iron.constraint.string.Match
import squants.MetricSystem
import squants.space.{Length, LengthUnit, Nanometers}

import at.ac.oeaw.imba.gerlich.gerlib.geometry.Point3D
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.refinement.IllegalRefinement

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
  final case class FieldOfView(private[imaging] get: Int :| Not[Negative]) extends FieldOfViewLike
      derives Order:
    def getRawValue: Int :| Not[Negative] = get

  /** Helpers for working with fields of view */
  object FieldOfView:
    /** Safely try to lift ordinary integer into field of view wrapper. */
    def fromInt: Int => Either[String, FieldOfView] =
      NonnegativeInt.either.map(_.map(FieldOfView.apply))

    /** Wrap the given value as a field of view, if it's valid as one. */
    def parse: Parser[FieldOfView] =
      parseThroughNonnegativeInt("FieldOfView")(FieldOfView.apply)

    /** Lift an ordinary integer into field of view wrapper, erroring if invalid.
      */
    def unsafeLift: Int => FieldOfView = i =>
      NonnegativeInt.option(i) match {
      case None    => throw IllegalRefinement(i, s"Illegal value as field of view: $i")
      case Some(t) => FieldOfView(t)
      }
  end FieldOfView

  private[gerlib] type PositionNamePunctuation = StrictEqual['.'] | StrictEqual['-'] |
    StrictEqual['_']

  /** A position name character must be alphanumeric, a hyphen, or an underscore.
    */
  private[gerlib] type PositionNameCharacterConstraint = Digit | Letter | PositionNamePunctuation

  /** A position name must be nonempty and contain only certain characters; furthermore, it must not
    * start with a hyphen for potential ambiguity with a negative number. We also exclude a number
    * in scientific notation.
    */
  private type PositionNameConstraint = Not[Empty] & ForAll[PositionNameCharacterConstraint] &
    Not[Match["-?[0-9]+"]] & // Exclude integers.
    Not[Match["-?[0-9]+\\.[0-9]+"]] & // Exclude decimals.
    Not[
      Match["-?[0-9]\\.[0-9]+E-?[0-9]{1,3}"]
    ] & // Exclude decimal scientific notation.
    Not[Match["-?[0-9]E-?[0-9]{1,3}"]] // Exclude integer scientific notation.

  /** The name of a position / field of view is a string whose characters all fulfill the
    * constraint.
    */
  final case class PositionName(
      private[imaging] get: String :| PositionNameConstraint
  ) extends FieldOfViewLike derives Order

  /** Put instances here since the refinement is opaque, so underlying String won't be visible
    * elsewhere.
    */
  object PositionName:
    /** Inline variant when argument is inlineable */
    inline def apply(
        inline get: String :| PositionNameConstraint
    ): PositionName = new PositionName(get)

    /** Refine through [[scala.util.Either]] as the monadic type. */
    def parse: Parser[PositionName] =
      (s: String) =>
        s
          .refineEither[PositionNameConstraint]
          .bimap(
            msg => s"Could not refine string ($s) as position name: $msg",
            PositionName.apply
          )

    /** Refine the string, then wrap it. */
    def unsafe = (s: String) => parse(s).fold(msg => throw IllegalArgumentException(msg), identity)
  end PositionName

  // TODO: try to restrict the .symbol abstract member to be "px" singleton.
  opaque type PixelDefinition = LengthUnit

  /** A fundamental unit of length in imaging, the pixel */
  object PixelDefinition:
    /** Define a unit of length in pixels by specifying number of nanometers per pixel. */
    def tryToDefine(onePixelIs: Length): Either[String, PixelDefinition] =
      PositiveReal
        .either(onePixelIs.to(Nanometers))
        .bimap(
          msg => s"Cannot define pixel by given length ($onePixelIs): $msg",
          defineByNanometers
        )

    def unsafeDefine(onePixelIs: Length): PixelDefinition =
      tryToDefine(onePixelIs)
        .leftMap { msg => new Exception(msg) }
        .fold(throw _, identity)

    given Show[PixelDefinition] =
      Show.show(pxDef => s"PixelDefinition: ${pxDef(1)}")

    /** Define a unit of length in pixels by specifying number of nanometers per pixel. */
    private def defineByNanometers(nmPerPx: Double :| Positive): PixelDefinition = new:
      val conversionFactor: Double = nmPerPx * MetricSystem.Nano
      val symbol: String = "px"

    object syntax:
      extension (pxDef: PixelDefinition)
        def lift[A: Numeric](a: A): Length = (pxDef: LengthUnit).apply(a)
  end PixelDefinition

  /** Rescaling of the units in 3D */
  final case class Pixels3D(
      private val x: PixelDefinition,
      private val y: PixelDefinition,
      private val z: PixelDefinition
  ):
    import PixelDefinition.syntax.lift
    def liftX[A: Numeric](a: A): Length = x.lift(a)
    def liftY[A: Numeric](a: A): Length = y.lift(a)
    def liftZ[A: Numeric](a: A): Length = z.lift(a)
  end Pixels3D

  def euclideanDistanceBetweenImagePoints[C: Numeric](
      pixels: Pixels3D
  )(p: Point3D[C], q: Point3D[C]): Length =
    import scala.math.Numeric.Implicits.infixNumericOps
    val delX = pixels.liftX(p.x.value - q.x.value)
    val delY = pixels.liftY(p.y.value - q.y.value)
    val delZ = pixels.liftZ(p.z.value - q.z.value)
    val distanceSquared = List(delX, delY, delZ).foldLeft(0.0): (sumSqs, pxDiff) =>
      sumSqs + scala.math.pow(pxDiff to Nanometers, 2)
    Nanometers(scala.math.sqrt(distanceSquared))
end imaging
