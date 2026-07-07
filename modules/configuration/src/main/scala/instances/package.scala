package at.ac.oeaw.imba.gerlich.gerlib.configuration

/** Configuration-related typeclass instances */
package object instances:
  object all extends AllConfigurationInstances

  trait AllConfigurationInstances extends PureConfigImagingInstances, PureConfigSquantsInstances
end instances
