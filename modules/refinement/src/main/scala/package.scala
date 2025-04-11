package at.ac.oeaw.imba.gerlich.gerlib

import cats.syntax.all.*

package object refinement:
  /** Error subtype for when refinement of a negative integer as nonnegative is attempted
    *
    * @param rawValue
    *   The value to refine
    * @param msg The initial refinement failure message
    * @param context
    *   The context (e.g., purpose) in which the refinement's being attempted; this is used to craft
    *   a more informative error message
    */
  final case class IllegalRefinement[A] private[refinement](
      rawValue: A,
      msg: String,
      context: Option[String]
  ) extends IllegalArgumentException(
        s"Cannot refine value $rawValue: $msg.${context
            .fold("")(ctx => s" Context: $ctx")}"
      )

  /** Builders for [[IllegalRefinement]] */
  object IllegalRefinement:
    /** No additional message context */
    def apply[A](value: A, msg: String): IllegalRefinement[A] =
      new IllegalRefinement(value, msg, None)

    /** Add additional message context. */
    private[refinement] def apply[A](
        value: A,
        msg: String,
        ctx: String
    ): IllegalRefinement[A] =
      new IllegalRefinement(value, msg, ctx.some)
  end IllegalRefinement
end refinement
