package at.ac.oeaw.imba.gerlich.gerlib

import cats.*
import cats.syntax.all.*

/** Like [[cats.Show]], but sometimes simpler. */
trait SimpleShow[T]:
  def show_(t: T): String

/** Helper functions and typeclass instances */
object SimpleShow:
  /** Use a [[cats.Show]] instance. */
  def fromShow[A: Show]: SimpleShow[A] = new:
    override def show_(a: A): String = a.show

  /** Use a value's {@code toString} method. */
  def fromToString[A]: SimpleShow[A] = new:
    override def show_(a: A): String = a.toString

  /** Use the given function. */
  def instance[A](f: A => String): SimpleShow[A] = new:
    override def show_(t: A): String = f(t)

  /** Use the given function to get a value for which an instance already is available.
    */
  given Contravariant[SimpleShow] with
    override def contramap[A, B](fa: SimpleShow[A])(
        f: B => A
    ): SimpleShow[B] = new:
      override def show_(b: B): String = fa.show_(f(b))
