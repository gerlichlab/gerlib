package at.ac.oeaw.imba.gerlich.gerlib.imaging

package object instances:
  object all extends AllInstances

  trait AllInstances
      extends FieldOfViewLikeInstances,
        ImagingChannelInstances,
        ImagingTimepointInstances
