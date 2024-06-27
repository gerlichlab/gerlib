package at.ac.oeaw.imba.gerlich.gerlib.testing

import cats.syntax.all.*
import org.scalacheck.*

import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

trait ImagingInstances extends CatsScalacheckInstances:
    /** [[org.scalacheck.Arbitrary]] instance for generating a [[at.ac.oeaw.imba.gerlich.gerlib.imaging.FieldOfView]] value */
    given arbitraryForFieldOfView(using arbNN: Arbitrary[NonnegativeInt]): Arbitrary[FieldOfView] = arbNN.map(FieldOfView.apply)

    /** [[org.scalacheck.Arbitrary]] instance for generating a [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingChannel]] value */
    given arbitraryForImagingChannel(using arbNN: Arbitrary[NonnegativeInt]): Arbitrary[ImagingChannel] = arbNN.map(ImagingChannel.apply)
    
    /** [[org.scalacheck.Arbitrary]] instance for generating a [[at.ac.oeaw.imba.gerlich.gerlib.imaging.ImagingTimepoint]] value */
    given arbitraryForImagingTimepoint(using arbNN: Arbitrary[NonnegativeInt]): Arbitrary[ImagingTimepoint] = arbNN.map(ImagingTimepoint.apply)
end ImagingInstances
