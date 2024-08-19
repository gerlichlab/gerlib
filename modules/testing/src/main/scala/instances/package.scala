package at.ac.oeaw.imba.gerlich.gerlib.testing

package object instances:
  object all extends AllInstances

  trait AllInstances
      extends CatsScalacheckInstances,
        CellInstances,
        CsvInstances,
        GeometricInstances,
        ImagingInstances,
        NumericInstances,
        RoiInstances
