package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.data.NonEmptyList
import cats.syntax.all.*
import org.scalacheck.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.StrictEqual
import io.github.iltotore.iron.constraint.char.{Digit, Letter}

import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*
import at.ac.oeaw.imba.gerlich.gerlib.testing.syntax.scalacheck.*

/** Scalacheck typeclass instances for some of the imaging datatypes */
trait ImagingInstances extends CatsScalacheckInstances:
  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.FieldOfView]] value
    */
  given arbitraryForFieldOfView(using
      arbNN: Arbitrary[NonnegativeInt]
  ): Arbitrary[FieldOfView] = arbNN.map(FieldOfView.apply)

  /** Generate always the hyphen, allowed as one of the position name
    * characters.
    */
  private def genPositionNamePunctuation
      : Gen[Char :| PositionNameCharacterConstraint] =
    import io.github.iltotore.iron.autoRefine
    Gen.oneOf('.', '-', '_')

  /** Choose each character from among the valid pool, then concatenate. */
  given arbitraryForPositionName(using
      arbLetter: Arbitrary[Char :| Letter],
      arbDigit: Arbitrary[Char :| Digit]
  ): Arbitrary[PositionName] =
    import io.github.iltotore.iron.cats.given
    type GoodChar = Char :| PositionNameCharacterConstraint
    given Arbitrary[GoodChar] = Gen
      .oneOf(
        arbLetter.arbitrary.map(_.asInstanceOf[GoodChar]),
        arbDigit.arbitrary.map(_.asInstanceOf[GoodChar]),
        genPositionNamePunctuation
      )
      .toArbitrary
    summon[Arbitrary[NonEmptyList[GoodChar]]]
      .suchThat(_.exists(_.isLetter))
      .map(_.mkString_(""))
      .map(PositionName.unsafe)

  /** Simply choose from one of the given instances. */
  given arbitraryForFieldOfViewLike(using
      arbFov: Arbitrary[FieldOfView],
      arbPos: Arbitrary[PositionName]
  ): Arbitrary[FieldOfViewLike] =
    Arbitrary { Gen.oneOf(arbFov.arbitrary, arbPos.arbitrary) }

  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingChannel]] value
    */
  given arbitraryForImagingChannel(using
      arbNN: Arbitrary[NonnegativeInt]
  ): Arbitrary[ImagingChannel] = arbNN.map(ImagingChannel.apply)

  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingTimepoint]] value
    */
  given arbitraryForImagingTimepoint(using
      arbNN: Arbitrary[NonnegativeInt]
  ): Arbitrary[ImagingTimepoint] = arbNN.map(ImagingTimepoint.apply)

  given arbitraryForImagingContext(using
      arbFov: Arbitrary[FieldOfViewLike],
      arbTime: Arbitrary[ImagingTimepoint],
      arbChannel: Arbitrary[ImagingChannel]
  ): Arbitrary[ImagingContext] =
    (arbFov, arbTime, arbChannel).mapN(ImagingContext.apply)
end ImagingInstances
