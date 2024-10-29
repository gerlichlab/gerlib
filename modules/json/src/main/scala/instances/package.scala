package at.ac.oeaw.imba.gerlich.gerlib.json

package object instances:
  object all extends AllJsonInstances

  trait AllJsonInstances
      extends JsonInstancesForCollections,
        JsonInstancesForGeometry,
        JsonInstancesForNumeric,
        JsonInstancesForText
