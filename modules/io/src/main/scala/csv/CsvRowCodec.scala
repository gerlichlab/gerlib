package at.ac.oeaw.imba.gerlich.gerlib.io.csv
package csv

import fs2.data.csv.*

/** Bundler of CSV row decoder and encoder for a particular data type */
trait CsvRowCodec[T]:
  private type Row = RowF[Some, String]
  def decode: Row => DecoderResult[T]
  def encode: T => Row
end CsvRowCodec

/** Helpers for working with CSV row codecs */
object CsvRowCodec:
  /** Use the given decoder and encoder to define the necessary functions. */
  def apply[T](
      decoder: CsvRowDecoder[T, String],
      encoder: CsvRowEncoder[T, String]
  ): CsvRowCodec[T] = new:
    override def decode = decoder.apply
    override def encode = encoder.apply

  given csvRowCodecFromComponents[T](using
      dec: CsvRowDecoder[T, String],
      enc: CsvRowEncoder[T, String]
  ): CsvRowCodec[T] =
    apply(dec, enc)
end CsvRowCodec
