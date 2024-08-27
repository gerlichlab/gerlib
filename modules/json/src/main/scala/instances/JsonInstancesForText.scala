package at.ac.oeaw.imba.gerlich.gerlib.json
package instances

/** JSON-related typeclass instances for text-like data types */
trait JsonInstancesForText:
  /** Simple write the String value as itself, wrapped as JSON. */
  given JsonValueWriter[String, ujson.Str] =
    JsonValueWriter.instance(ujson.Str.apply)
end JsonInstancesForText
