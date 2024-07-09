package at.ac.oeaw.imba.gerlich.gerlib
package syntax

trait SyntaxForSimpleShow:
  extension [T](t: T)(using ev: SimpleShow[T])
    def `show_` : String = ev.show_(t)
