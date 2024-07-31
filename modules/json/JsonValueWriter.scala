package at.ac.oeaw.imba.gerlich.gerlib
package json

import cats.*

/** Evidence that a {@code A} can be represented as a [[ujson.Value]] value */
trait JsonValueWriter[I, +O <: ujson.Value]:
  /** Represent the given value as a [[ujson.Value]] value. */
  def apply(i: I): O
end JsonValueWriter

/** Provide instances for natural, core types. */
object JsonValueWriter:
  def apply[I, O <: ujson.Value](f: I => O): JsonValueWriter[I, O] = new:
    override def apply(i: I): O = f(i)

  def instance[I, O <: ujson.Value](f: I => O): JsonValueWriter[I, O] = apply(f)

  given contravariantForJsonValueWriter[O <: ujson.Value]
      : Contravariant[[I] =>> JsonValueWriter[I, O]] with
    override def contramap[A, B](writeA: JsonValueWriter[A, O])(
        f: B => A
    ): JsonValueWriter[B, O] = new:
      override def apply(i: B): O = writeA(f(i))

  given JsonValueWriter[Int, ujson.Num] with
    override def apply(a: Int): ujson.Num = ujson.Num(a)

  given JsonValueWriter[Boolean, ujson.Bool] with
    override def apply(a: Boolean): ujson.Bool = ujson.Bool(a)

  given JsonValueWriter[String, ujson.Str] with
    override def apply(a: String): ujson.Str = ujson.Str(a)
end JsonValueWriter
