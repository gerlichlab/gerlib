package at.ac.oeaw.imba.gerlich.gerlib.geometry

/** Typeclass instances for geometry-related data types */
package object instances:
  /** Aggregation of all the geometry-related data types' typeclass instances, for import
    * convenience with .all.given
    */
  object all extends AllGeometryInstances

  /** Aggregation of all the geometry-related data types' typeclass instances */
  trait AllGeometryInstances extends CoordinateInstances, PointInstances
end instances
