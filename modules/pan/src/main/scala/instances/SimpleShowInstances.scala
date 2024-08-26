package at.ac.oeaw.imba.gerlich.gerlib
package instances

/** {@code SimpleShow} instances for core/builtin Scala types */
trait SimpleShowInstances:

  /** Simply print the numeric value as-is. */
  given SimpleShow[Double] = SimpleShow.fromToString

  /** Show the integer by its {@code toString} representation. */
  given SimpleShow[Int] = SimpleShow.fromToString

  /** Show a {@code String} as identity. */
  given SimpleShow[String] = SimpleShow.instance(identity)
