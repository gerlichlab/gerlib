package at.ac.oeaw.imba.gerlich.gerlib.zarr;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import ucar.ma2.InvalidRangeException;
import com.bc.zarr.ZarrArray;
import com.bc.zarr.ZarrGroup;
import com.bc.zarr.ZarrUtils;

/** Thin wrapper around tiny subset of jzarr API, for better interop with Scala */
public class JzarrTools {
    /**
     * Read data block from given position in given array.
     * 
     * This reads directly into an array of int, to avoid the potential for 
     * undefined/bad behavior when reading unsigned 2-byte integer intto 
     * Java short, which is how Jzarr currently handles this. 
     * https://github.com/zarr-developers/jzarr/blob/main/src/main/java/com/bc/zarr/ZarrUtils.java#L199-L207
     * 
     * @param z The array from which to read data
     * @param shape The shape of data chunk to read
     * @param offset Where in the array data should be read from
     * @return An array of the given shape, starting at the given position, from the given array
     * @throws IOException
     * @throws InvalidRangeException
     */
    public static int[] readFrom(ZarrArray z, int[] shape, int[] offset) throws IOException, InvalidRangeException {
        int flatSize = ZarrUtils.computeSizeInteger(shape);
        int[] result = new int[flatSize];
        z.read(result, shape, offset);
        return result;
    }
}
