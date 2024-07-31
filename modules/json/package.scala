package at.ac.oeaw.imba.gerlich.gerlib

/** Functionality for working with JSON */
package object json:
  object syntax:
    extension [I](i: I)
      /** Represent the syntax-enriched value as a [[ujson.Value]] value. */
      def asJson(using write: JsonValueWriter[I, ujson.Value]): ujson.Value =
        write(i)
