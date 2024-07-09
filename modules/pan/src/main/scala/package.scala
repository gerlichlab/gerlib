package at.ac.oeaw.imba.gerlich

import cats.*
import cats.syntax.all.*

package object gerlib:

  trait SimpleShow[T]:
    def show_(t: T): String

  object SimpleShow:
    def fromShow[A: Show]: SimpleShow[A] = new:
      override def show_(a: A): String = a.show
    def fromToString[A]: SimpleShow[A] = new:
      override def show_(a: A): String = a.toString
    def instance[A](f: A => String): SimpleShow[A] = new:
      override def show_(t: A): String = f(t)
    given Contravariant[SimpleShow] with
      override def contramap[A, B](fa: SimpleShow[A])(
          f: B => A
      ): SimpleShow[B] = new:
        override def show_(b: B): String = fa.show_(f(b))
