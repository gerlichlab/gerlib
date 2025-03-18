package at.ac.oeaw.imba.gerlich.gerlib.testing
package instances

import cats.syntax.all.*
import org.scalacheck.*

import io.github.iltotore.iron.:|
import io.github.iltotore.iron.constraint.any.StrictEqual
import io.github.iltotore.iron.constraint.char.{Digit, Letter}

import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Scalacheck typeclass instances for some of the imaging datatypes */
trait ImagingInstances extends CatsScalacheckInstances:
  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.FieldOfView]] value
    */
  given (arbNN: Arbitrary[NonnegativeInt]) => Arbitrary[FieldOfView] = arbNN.map(FieldOfView.apply)

  /** Use the given instances of letter or digit generators as the base. */
  given (Arbitrary[Char :| Letter], Arbitrary[Char :| Digit]) => Arbitrary[PositionName] =
    type GoodChar = Char :| PositionNameCharacterConstraint
    val goodPunctuation: Set[GoodChar] =
      import io.github.iltotore.iron.autoRefine
      Set('-', '.', '_')
    def genPunct: Gen[GoodChar] = Gen.oneOf(goodPunctuation)
    def genPosNameChar: Gen[GoodChar] = Gen.oneOf(
      Arbitrary.arbitrary[Char :| Letter].map(_.asInstanceOf[GoodChar]),
      Arbitrary.arbitrary[Char :| Digit].map(_.asInstanceOf[GoodChar]),
      genPunct
    )
    Arbitrary {
      Gen
        .nonEmptyListOf(genPosNameChar)
        .map(chars => chars.mkString(""))
        .suchThat { s =>
          s.toList.filter(_.isLetter) match
          case Nil =>
            !(
              // Check that the string doesn't encode an integer or decimal.
              raw"-?[0-9]+".r.matches(s) ||
                raw"-?[0-9]+\.[0-9]+".r.matches(s)
            )
          case 'E' :: Nil =>
            !(
              // Check that the string isn't a scientific notation.
              raw"-?[0-9]\.[0-9]+E-?[0-9]{1,3}".r.matches(s) ||
                raw"-?[0-9]E-?[0-9]{1,3}".r.matches(s)
            )
          case _ => true
        }
        .map(PositionName.unsafe)
    }

  /** Simply choose from one of the given instances. */
  given (
      arbFov: Arbitrary[FieldOfView],
      arbPos: Arbitrary[PositionName]
  ) => Arbitrary[FieldOfViewLike] =
    Arbitrary:
      Gen.oneOf(arbFov.arbitrary, arbPos.arbitrary)

  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingChannel]] value
    */
  given (
      arbNN: Arbitrary[NonnegativeInt]
  ) => Arbitrary[ImagingChannel] = arbNN.map(ImagingChannel.apply)

  /** [[org.scalacheck.Arbitrary]] instance for generating a
    * [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingTimepoint]] value
    */
  given (
      arbNN: Arbitrary[NonnegativeInt]
  ) => Arbitrary[ImagingTimepoint] = arbNN.map(ImagingTimepoint.apply)

  given (
      arbFov: Arbitrary[FieldOfViewLike],
      arbTime: Arbitrary[ImagingTimepoint],
      arbChannel: Arbitrary[ImagingChannel]
  ) => Arbitrary[ImagingContext] =
    (arbFov, arbTime, arbChannel).mapN(ImagingContext.apply)
end ImagingInstances
