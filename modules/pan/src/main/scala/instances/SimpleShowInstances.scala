package at.ac.oeaw.imba.gerlich.gerlib
package instances

trait SimpleShowInstances:
    given SimpleShow[Int] = SimpleShow.fromToString
    given SimpleShow[String] = SimpleShow.instance(identity)
