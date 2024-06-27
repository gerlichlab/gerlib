package at.ac.oeaw.imba.gerlich.gerlib.geometry

/** Typesafe representation of a point in 3D space */
final case class Point3D[C](x: XCoordinate[C], y: YCoordinate[C], z: ZCoordinate[C])
