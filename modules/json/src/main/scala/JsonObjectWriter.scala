package at.ac.oeaw.imba.gerlich.gerlib.json

trait JsonObjectWriter[A]:
  def apply(a: A): ujson.Obj
end JsonObjectWriter
