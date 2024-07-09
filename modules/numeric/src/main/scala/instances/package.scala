package at.ac.oeaw.imba.gerlich.gerlib.numeric

package object instances:
  object all extends AllInstances

  trait AllInstances
      extends NonnegativeIntInstances,
        NonnegativeRealInstances,
        PositiveIntInstances,
        PositiveRealInstances
