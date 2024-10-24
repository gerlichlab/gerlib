package at.ac.oeaw.imba.gerlich.gerlib.geometry

/** Type wrapper around a numeric value representing a spatial coordinate */
sealed trait Coordinate[A]:
  /** Access the underling, wrapped value. */
  def value: A

/** Wrap the given value as reprsenting a x-coordinate in space. */
final case class XCoordinate[A](value: A) extends Coordinate[A]

/** Wrap the given value as reprsenting a y-coordinate in space. */
final case class YCoordinate[A](value: A) extends Coordinate[A]

/** Wrap the given value as reprsenting a z-coordinate in space. */
final case class ZCoordinate[A](value: A) extends Coordinate[A]
