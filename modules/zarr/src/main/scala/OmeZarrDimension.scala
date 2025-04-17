package at.ac.oeaw.imba.gerlich.gerlib.zarr

import scala.jdk.CollectionConverters.*
import scala.util.Try
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all.*
import mouse.boolean.*

import com.bc.zarr.ZarrGroup

import at.ac.oeaw.imba.gerlich.gerlib.numeric.NonnegativeInt
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.toRawIndex
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrBlockSize
import at.ac.oeaw.imba.gerlich.gerlib.zarr.OmeZarrIndex.OmeZarrStandardCoordinate

/** Units of length */
enum UnitsLength(val fullName: String):
  /** Micrometer */
  case Micrometer extends UnitsLength("micrometer")

  /** Nanometer */
  case Nanometer extends UnitsLength("nanometer")

object UnitsLength:
  def fromString(s: String): Option[UnitsLength] =
    UnitsLength.values.find(_.toString.equalsIgnoreCase(s))

/** The different OME ZARR axes/dimensions */
sealed trait OmeZarrDimension:
  def nameOpt: Option[OmeZarr.DimensionName]

/** OME ZARR axis/dimension for spatial quantities, admitting a length */
sealed trait SpatialOmeZarrDimension extends OmeZarrDimension:
  def getUnit: UnitsLength

/** Helpers for working with Open Microscopy (OME) dimensions */
object OmeZarr:
  /** Key in 'axes' mapping for which the value encodes ZARR dimension name */
  private val nameKey = "name"

  /** Key in 'axes' mapping for which the value encodes ZARR dimension type, e.g. "space"
    */
  private val typeKey = "type"

  /** Key in 'axes' mapping for which the value encodes ZARR dimension unit of measure, e.g.
    * "micrometer"
    */
  private val unitKey = "unit"

  /** Name of a ZARR dimension */
  opaque type DimensionName = String

  /** Helpers for working with name of a ZARR dimension */
  object DimensionName:
    /** Wrap the given string as a ZARR dimension name. */
    def liftString(s: String): DimensionName = (s: DimensionName)

    /** Lift an optional raw value into the wrappr. */
    def liftOption(opt: Option[String]): Option[DimensionName] =
      opt.map(liftString)

  /** ZARR dimension for timepoint of an imaging experiment */
  case class TimepointDimension(nameOpt: Option[DimensionName]) extends OmeZarrDimension

  /** ZARR dimension for channel of an image */
  case class ChannelDimension(nameOpt: Option[DimensionName]) extends OmeZarrDimension

  /** ZARR dimension for z axis of an image stack */
  case class ZDimension(getUnit: UnitsLength) extends SpatialOmeZarrDimension:
    override def nameOpt: Option[DimensionName] = "z".some

  /** ZARR dimension for y axis of an image */
  case class YDimension(getUnit: UnitsLength) extends SpatialOmeZarrDimension:
    override def nameOpt: Option[DimensionName] = "y".some

  /** ZARR dimension for x axis of an image */
  case class XDimension(getUnit: UnitsLength) extends SpatialOmeZarrDimension:
    override def nameOpt: Option[DimensionName] = "x".some

  /** Build function that attempt to cast a given object to a target type.
    *
    * If the cast succeeds, its result will be wrapped and returned; otherwise, an error message
    * will be contextualised by the given descriptive argument here, and wrapped and returned.
    *
    * @param targetDescription
    *   The contextual information with which to enrich potential error message
    * @return
    *   See above
    */
  private def safeCast[O](targetDescription: String): Any => Either[String, O] =
    i =>
      Try:
        i.asInstanceOf[O]
      .toEither.leftMap {
        case e: ClassCastException =>
          s"Could not complete type cast ($targetDescription): ${e.getMessage}"
        case e: Throwable => throw e
      }

  final case class IndexMapping private (
      timeIndex: NonnegativeInt,
      channelIndex: NonnegativeInt,
      zIndex: NonnegativeInt,
      yIndex: NonnegativeInt,
      xIndex: NonnegativeInt
  ):
    def buildOriginIndex(coordinate: OmeZarrStandardCoordinate): Array[Int] =
      Array(
        timeIndex -> coordinate.getTime.toRawIndex,
        channelIndex -> coordinate.getChannel.toRawIndex,
        zIndex -> coordinate.getZ.toRawIndex,
        yIndex -> coordinate.getY.toRawIndex,
        xIndex -> coordinate.getX.toRawIndex
      ).sortBy(_._1).map(_._2)

    def buildSizeIndex(coordinate: OmeZarrBlockSize): Array[Int] = Array(
      timeIndex -> coordinate.getTimeLength.toRawIndex,
      channelIndex -> coordinate.getChannelLength.toRawIndex,
      zIndex -> coordinate.getZLength.toRawIndex,
      yIndex -> coordinate.getYLength.toRawIndex,
      xIndex -> coordinate.getXLength.toRawIndex
    ).sortBy(_._1).map(_._2)

  private val timeName = "time"
  private val channelName = "channel"
  private val zName = "z"
  private val yName = "y"
  private val xName = "x"

  /** Helpers for working with mapping of OME-ZARR dimensions to array index */
  object IndexMapping:
    import OmeZarr.*

    /** Attempt to get positional index for each OME-ZARR dimension, according to the metadata
      * associated with the given ZARR group.
      *
      * @param zg
      *   The ZARR group with metadata to parse
      * @return
      *   Either a [[scala.util.Left]]-wrapped collection of error/problem messages, or a
      *   [[scala.util.Right]]-wrapped parsed mapping
      */
    def fromZarrGroup(
        zg: ZarrGroup
    ): Either[NonEmptyList[String], IndexMapping] =
      getRawAxesMaps(zg)
        .leftMap(NonEmptyList.one)
        .flatMap(getOmeZarrDimensionMap)

    def fromDimensions(
        dims: NonEmptyList[OmeZarrDimension]
    ): Either[NonEmptyList[String], IndexMapping] =
      val indexedDims = NonnegativeInt.indexed(dims)
      if indexedDims.length =!= 5
      then
        NonEmptyList
          .one(s"Expected 5 dimensions but got ${indexedDims.length}")
          .asLeft
      else
        val tNel = getUniqueIndexFor(timeName)(
          _.isInstanceOf[TimepointDimension]
        )(indexedDims).toValidatedNel
        val cNel = getUniqueIndexFor(channelName)(
          _.isInstanceOf[ChannelDimension]
        )(indexedDims).toValidatedNel
        val zNel = getUniqueIndexFor("z")(_.isInstanceOf[ZDimension])(
          indexedDims
        ).toValidatedNel
        val yNel = getUniqueIndexFor("y")(_.isInstanceOf[YDimension])(
          indexedDims
        ).toValidatedNel
        val xNel = getUniqueIndexFor("x")(_.isInstanceOf[XDimension])(
          indexedDims
        ).toValidatedNel
        (tNel, cNel, zNel, yNel, xNel).tupled.toEither.flatMap { (t, c, z, y, x) =>
          val reps = List(t, c, z, y, x)
            .groupBy(identity)
            .view
            .mapValues(_.length)
            .filter(_._2 > 1)
            .toMap
          reps.isEmpty.either(
            NonEmptyList.one(s"Repeated dimension index counts: $reps"),
            IndexMapping(t, c, z, y, x)
          )
        }

    private def getUniqueIndexFor(
        targetName: String
    )(isMatch: OmeZarrDimension => Boolean): NonEmptyList[
      (OmeZarrDimension, NonnegativeInt)
    ] => Either[String, NonnegativeInt] =
      dims =>
        (
          dims.filter((d, _) => isMatch(d)) match
          case Nil           => "No match".asLeft
          case (_, i) :: Nil => i.asRight
          case multi         => s"${multi.length} matches".asLeft
        ).leftMap(msg => s"$targetName: $msg")
  end IndexMapping

  private def getOmeZarrDimensionMap(
      rawAxes: List[Map[String, String]]
  ): Either[NonEmptyList[String], IndexMapping] = for
    nonemptyAxes <- rawAxes.toNel.toRight(NonEmptyList.one("Empty axes!"))
    dimensions <- nonemptyAxes.traverse(parseDimension)
    mapping <- IndexMapping.fromDimensions(dimensions)
  yield mapping

  /** Parse a single axis/dimension's metadata mapping.
    *
    * @param m
    *   The metadata to parse
    * @return
    *   Either a [[scala.util.Left]]-wrapped nonempty list of explanations of why the metadata parse
    *   failed, or a [[scala.util.Right]]-wrapped dimension entity
    */
  private def parseDimension(
      m: Map[String, String]
  ): Either[NonEmptyList[String], OmeZarrDimension] =
    type BuildSpatial = UnitsLength => SpatialOmeZarrDimension
    val nameOptNel = m.get(nameKey).validNel[String]
    val typeNel = m.get(typeKey).toValidNel(s"Missing type key ('$typeKey')")
    val unitOptNel = m.get(unitKey).validNel[String]
    (nameOptNel, typeNel, unitOptNel).tupled.toEither.flatMap {
      /* First, deal with all the time/channel possibilities... */
      case (nameOpt, `timeName`, None) =>
        TimepointDimension(DimensionName.liftOption(nameOpt)).asRight
      case (nameOpt, `channelName`, None) =>
        ChannelDimension(DimensionName.liftOption(nameOpt)).asRight
      case (_, typename @ (`timeName` | `channelName`), Some(unitName)) =>
        NonEmptyList
          .one(
            s"Cannot have unit name for dimension type '$typename'; got: '$unitName'"
          )
          .asLeft
      /* ...then, deal with all the spatial possibilities. */
      case (dimOpt, "space", unitOpt) =>
        val buildNel: ValidatedNel[String, BuildSpatial] =
          dimOpt match
          case Some(`zName`) => (ZDimension.apply).validNel[String]
          case Some(`yName`) => (YDimension.apply).validNel[String]
          case Some(`xName`) => (XDimension.apply).validNel[String]
          case Some(dimname) =>
            s"Illegal name for spatial dimension: $dimname"
              .invalidNel[BuildSpatial]
          case None =>
            "To decode spatial dimension, name is required"
              .invalidNel[BuildSpatial]
        val unitNel: ValidatedNel[String, UnitsLength] = unitOpt
          .toRight("For spatial dimension, unit is required")
          .flatMap(unitName =>
            UnitsLength
              .fromString(unitName)
              .toRight(s"Cannot decode as length units name: $unitName")
          )
          .toValidatedNel
        (buildNel, unitNel).mapN((build, unitName) => build(unitName)).toEither
      case (_, typename, _) =>
        NonEmptyList.one(s"Illegal ZARR dimension type: $typename").asLeft
    }

  /** Access the mapping of axes/dimensions metadata for a ZARR group.
    *
    * NB: This assumes that the data are stored as multiscale.
    *
    * Example data:
    *
    * { "multiscales" : [ { "metadata" : { "method" : "loci.common.image.SimpleImageScaler",
    * "version" : "Bio-Formats 7.3.0" }, "axes" : [ { "name" : "t", "type" : "time" }, { "name" :
    * "c", "type" : "channel" }, { "unit" : "micrometer", "name" : "z", "type" : "space" }, { "unit"
    * : "micrometer", "name" : "y", "type" : "space" }, { "unit" : "micrometer", "name" : "x",
    * "type" : "space" } ], "name" : "Scene #01", "datasets" : [ { "path" : "0",
    * "coordinateTransformations" : [ { "scale" : [ 1.0, 1.0, 0.18999999999999995,
    * 0.07059082892416223, 0.07059082892416223 ], "type" : "scale" } ] } ], "version" : "0.4" } ],
    * ..., ..., }
    *
    * @param zg
    *   The ZARR group for which associated axis/dimension metadata is desired
    * @return
    *   Either a [[scala.util.Left]]-wrapped explanation of why the metadata parse failed, or a
    *   [[scala.util.Right]]-wrapped list of raw (string-to-string) mappings, each representing a
    *   single axis/dimension
    */
  private def getRawAxesMaps(
      zg: ZarrGroup
  ): Either[String, List[Map[String, String]]] = for
    attrs <- (zg.getAttributes() != null)
      .either("No attributes on ZARR group", zg.getAttributes().asScala)
    rawMultiscales <- attrs
      .get("multiscales")
      .toRight("Missing 'multiscales' key")
      .flatMap(safeCast[java.util.List[Object]]("raw multiscales to list"))
      .map(_.asScala.toList)
    multiscales <- rawMultiscales match
    case Nil => "Multiscales list is empty".asLeft
    case h :: Nil =>
      safeCast[java.util.HashMap[String, Object]](
        "multiscales singleton to mapping"
      )(h)
    case multi =>
      s"Multiple (${multi.length}) entries in multiscales list, not just 1".asLeft
    rawAxes <- multiscales.asScala
      .get("axes")
      .toRight("Missing 'axes' key in multiscales metadata")
    axes <- safeCast[java.util.List[java.util.HashMap[String, String]]](
      "raw axes to list-of-map"
    )(rawAxes)
  yield axes.asScala.toList.map(_.asScala.toMap)
end OmeZarr
