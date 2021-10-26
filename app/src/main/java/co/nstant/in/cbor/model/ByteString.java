package co.nstant.in.cbor.model;

import java.util.Arrays;

public class ByteString extends ChunkableDataItem {

    private final byte[] bytes;

    public ByteString(byte[] bytes) {
        super(MajorType.BYTE_STRING);
        if (bytes == null) {
            this.bytes = null;
        } else {
            this.bytes = bytes;
        }
    }

    public byte[] getBytes() {
        if (bytes == null) {
            return null;
        } else {
            return bytes;
        }
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof ByteString) {
            ByteString other = (ByteString) object;
            return super.equals(object) && Arrays.equals(bytes, other.bytes);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ Arrays.hashCode(bytes);
    }

}
