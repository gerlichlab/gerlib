package at.ac.oeaw.imba.gerlich.gerlib.imaging

import cats.*
import cats.derived.*
import cats.syntax.all.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

// Seems to be needed to get the Order[Int :| Nonnegative] instance
import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt.given

/** Type wrapper around 0-based index of field of view (FOV) */
final case class FieldOfView(get: NonnegativeInt) derives Order

/** Helpers for working with fields of view */
object FieldOfView:
    /** Wrap the given value as a field of view, if it's valid as one. */
    def parse: String => Either[String, FieldOfView] = 
        parseThroughNonnegativeInt("FieldOfView")(FieldOfView.apply)
    given showForFieldOfView: Show[FieldOfView] = Show.show(_.get.show)
end FieldOfView
