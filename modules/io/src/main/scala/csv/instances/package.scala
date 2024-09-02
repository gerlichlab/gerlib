package at.ac.oeaw.imba.gerlich.gerlib.io.csv

package object instances:
  object all extends AllInstances

  trait AllInstances
      extends InstancesForCell,
        InstancesForEncoding,
        InstancesForGeometry,
        InstancesForImaging,
        InstancesForNumeric,
        InstancesForRoi
