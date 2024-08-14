package at.ac.oeaw.imba.gerlich.gerlib

/** Tools and types related to testing */
package object testing:
  object all extends AllInstances

  trait AllInstances
      extends CatsScalacheckInstances,
        CsvInstances,
        GeometricInstances,
        ImagingInstances,
        NumericInstances,
        RoiInstances
