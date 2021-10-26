package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import com.sparrowwallet.hummingbird.UR;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.util.Arrays;

public abstract class RegistryItem implements CborSerializable {
    public abstract RegistryType getRegistryType();

    public UR toUR() {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            CborEncoder encoder = new CborEncoder(baos);
            encoder.encode(toCbor());
            return new UR(getRegistryType(), baos.toByteArray());
        } catch(CborException | UR.InvalidTypeException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * <p>
     * The regular {@link BigInteger#toByteArray()} includes the sign bit of the number and
     * might result in an extra byte addition. This method removes this extra byte.
     * </p>
     * @param b the integer to format into a byte array
     * @param numBytes the desired size of the resulting byte array
     * @return numBytes byte long array.
     */
    protected static byte[] bigIntegerToBytes(BigInteger b, int numBytes) {
        if(b.signum() < 0) {
            throw new IllegalArgumentException("b must be positive or zero");
        }
        if(numBytes <= 0) {
            throw new IllegalArgumentException("numBytes must be positive");
        }
        byte[] src = b.toByteArray();
        byte[] dest = new byte[numBytes];
        boolean isFirstByteOnlyForSign = src[0] == 0;
        int length = isFirstByteOnlyForSign ? src.length - 1 : src.length;
        if(length > numBytes) {
            throw new IllegalArgumentException("The given number does not fit in " + numBytes);
        }
        int srcPos = isFirstByteOnlyForSign ? 1 : 0;
        int destPos = numBytes - length;
        System.arraycopy(src, srcPos, dest, destPos, length);
        Arrays.fill(src, (byte)0);
        return dest;
    }
}
