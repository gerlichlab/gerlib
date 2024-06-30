package at.ac.oeaw.imba.gerlich.gerlib.zarr

import cats.*
import cats.derived.*
import cats.syntax.all.*

import at.ac.oeaw.imba.gerlich.gerlib.imaging.*
import at.ac.oeaw.imba.gerlich.gerlib.numeric.*

/** Helpers for working with indexing into ZARR */
object OmeZarrIndex:
    import io.github.iltotore.iron.autoRefine
    extension (i: Time | Channel | Z | Y | X | LengthTime | LengthChannel | LengthZ | LengthY | LengthX)
        def asInt: Int = i

    /** Index into time dimension of ZARR array */
    opaque type Time = NonnegativeInt
    
    object Time:
        def apply(t: ImagingTimepoint): OmeZarrIndex.Time = t.get

    /** Index into channel dimension of ZARR array */
    opaque type Channel = NonnegativeInt
    
    object Channel:
        def apply(c: ImagingChannel): OmeZarrIndex.Channel = c.get

    /** Index into z dimension of ZARR array */
    opaque type Z = NonnegativeInt

    object Z:
        def fromDouble: Double => Option[Z] = ((_: Double).toInt).andThen(NonnegativeInt.maybe)

    /** Index into y dimension of ZARR array */
    opaque type Y = NonnegativeInt
    
    object Y:
        def fromDouble: Double => Option[Y] = ((_: Double).toInt).andThen(NonnegativeInt.maybe)
    
    /** Index into x dimension of ZARR array */
    opaque type X = NonnegativeInt

    object X:
        def fromDouble: Double => Option[X] = ((_: Double).toInt).andThen(NonnegativeInt.maybe)

    /** Index of "point" in standard OME-ZARR coordinate space */
    final case class OmeZarrStandardCoordinate(getTime: Time, getChannel: Channel, getZ: Z, getY: Y, getX: X)

    /** Number of values along time axis */
    opaque type LengthTime = PositiveInt

    object LengthTime:
        def apply(t: PositiveInt): LengthTime = (t : LengthTime)

    /** Number of values along channel axis */
    opaque type LengthChannel = PositiveInt

    object LengthChannel:
        def apply(c: PositiveInt): LengthChannel = (c : LengthChannel)

    /** Number of values along z axis */
    opaque type LengthZ = PositiveInt

    object LengthZ:
        def apply(z: PositiveInt): LengthZ = (z : LengthZ)

    /** Number of values along y axis */
    opaque type LengthY = PositiveInt

    object LengthY:
        def apply(y: PositiveInt): LengthY = (y : LengthY)

    /** Number of values along x axis */
    opaque type LengthX = PositiveInt

    object LengthX:
        def apply(x: PositiveInt): LengthX = (x : LengthX)

    final case class SpatialVolume(getZLength: LengthZ, getYLength: LengthY, getXLength: LengthX)

    final case class OmeZarrBlockSize(
        getTimeLength: LengthTime, 
        getChannelLength: LengthChannel, 
        getZLength: LengthZ, 
        getYLength: LengthY, 
        getXLength: LengthX,
    )
end OmeZarrIndex