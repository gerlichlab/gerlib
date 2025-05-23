package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.derived.*
import cats.syntax.all.*

import io.github.iltotore.iron.{:|, refineEither}
import io.github.iltotore.iron.cats.given
import io.github.iltotore.iron.constraint.any.{Not, StrictEqual}
import io.github.iltotore.iron.constraint.char.{Digit, Letter}
import io.github.iltotore.iron.constraint.collection.{Empty, ForAll}
import io.github.iltotore.iron.constraint.numeric.Negative
import io.github.iltotore.iron.constraint.string.Match
import squants.MetricSystem
import squants.space.{Length, LengthUnit, Microns, Millimeters, Nanometers}

import at.ac.oeaw.imba.gerlich.gerlib.geometry.{Distance, EuclideanDistance, Point3D}
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

  // TODO: try typelevel restriction of the .symbol abstract member to be "px" singleton.
  type PixelDefinition = LengthUnit

  /** A fundamental unit of length in imaging, the pixel */
  object PixelDefinition:
    /** Define a unit of length in pixels by specifying a physical length per pixel. */
    def tryToDefine(l: Length): Either[String, PixelDefinition] = for
      baseFactor <- l.unit match {
      case Nanometers  => MetricSystem.Nano.asRight
      case Microns     => MetricSystem.Micro.asRight
      case Millimeters => MetricSystem.Milli.asRight
      case u => s"Cannot resolve base conversion factor for unit ($u) from length $l".asLeft
      }
      specificFactor <- PositiveReal.either(l.value)
    yield new:
      val conversionFactor: Double = specificFactor * baseFactor
      val symbol: String = "px"

    given Show[PixelDefinition] =
      Show.show(pxDef => s"PixelDefinition: ${pxDef(1)}")

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

  def euclideanDistanceBetweenImagePoints[A, C: Numeric](
      pixels: Pixels3D
  )(f: A => Point3D[C]): (A, A) => EuclideanDistance =
    (a1, a2) => euclideanDistanceBetweenImagePoints(pixels)(f(a1), f(a2))

  def euclideanDistanceBetweenImagePoints[C: Numeric](
      pixels: Pixels3D
  )(p: Point3D[C], q: Point3D[C]): EuclideanDistance =
    import scala.math.Numeric.Implicits.infixNumericOps
    computeDistance(
      pixels.liftX(p.x.value - q.x.value),
      pixels.liftY(p.y.value - q.y.value),
      pixels.liftZ(p.z.value - q.z.value)
    )

  private def computeDistance(delX: Length, delY: Length, delZ: Length): EuclideanDistance =
    val d = (delX * delX + delY * delY + delZ * delZ).squareRoot
    Distance.either(d).fold(msg => throw IllegalRefinement(d, msg), EuclideanDistance.apply)
end imaging
