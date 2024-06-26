package at.ac.oeaw.imba.gerlich.gerlib.testing

import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Default behaviors for a [[org.scalatestplus.scalacheck.ScalaCheckPropertyChecks]]-backed suite */
trait ScalacheckSuite extends ScalaCheckPropertyChecks: // This trait is only meaningful in a certain testing context.
    /** Automatically set the minimum number of passing examples to 100, rather than default of 10, for property-based tests. */
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100)
