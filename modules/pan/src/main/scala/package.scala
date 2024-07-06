package at.ac.oeaw.imba.gerlich

import cats.*
import cats.syntax.all.*

package object gerlib:

    trait SimpleShow[T]:
        def show_(t: T): String

    object SimpleShow:
        extension [T](t: T)(using ev: SimpleShow[T])
            def `show_`: String = ev.show_(t)
        def fromShow[A : Show]: SimpleShow[A] = new:
            override def show_(a: A): String = a.show
        def fromToString[A]: SimpleShow[A] = new:
            override def show_(a: A): String = a.toString
        given Contravariant[SimpleShow] with
            override def contramap[A, B](fa: SimpleShow[A])(f: B => A): SimpleShow[B] = new:
                override def show_(b: B): String = fa.show_(f(b))
