package co.nstant.in.cbor.encoder;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.AdditionalInformation;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Tag;

public abstract class AbstractEncoder<T> {

    private final OutputStream outputStream;
    protected final CborEncoder encoder;

    public AbstractEncoder(CborEncoder encoder, OutputStream outputStream) {
        this.encoder = encoder;
        this.outputStream = outputStream;
    }

    public abstract void encode(T dataItem) throws CborException;

    protected void encodeTypeChunked(MajorType majorType) throws CborException {
        int symbol = majorType.getValue() << 5;
        symbol |= AdditionalInformation.INDEFINITE.getValue();
        try {
            outputStream.write(symbol);
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
    }

    protected void encodeTypeAndLength(MajorType majorType, long length) throws CborException {
        int symbol = majorType.getValue() << 5;
        if (length <= 23L) {
            write((byte) (symbol | length));
        } else if (length <= 255L) {
            symbol |= AdditionalInformation.ONE_BYTE.getValue();
            write((byte) symbol, (byte) length);
        } else if (length <= 65535L) {
            symbol |= AdditionalInformation.TWO_BYTES.getValue();
            write((byte) symbol, (byte) (length >> 8), (byte) (length & 0xFF));
        } else if (length <= 4294967295L) {
            symbol |= AdditionalInformation.FOUR_BYTES.getValue();
            write((byte) symbol, (byte) ((length >> 24) & 0xFF), (byte) ((length >> 16) & 0xFF),
                (byte) ((length >> 8) & 0xFF), (byte) (length & 0xFF));
        } else {
            symbol |= AdditionalInformation.EIGHT_BYTES.getValue();
            write((byte) symbol, (byte) ((length >> 56) & 0xFF), (byte) ((length >> 48) & 0xFF),
                (byte) ((length >> 40) & 0xFF), (byte) ((length >> 32) & 0xFF), (byte) ((length >> 24) & 0xFF),
                (byte) ((length >> 16) & 0xFF), (byte) ((length >> 8) & 0xFF), (byte) (length & 0xFF));
        }
    }

    protected void encodeTypeAndLength(MajorType majorType, BigInteger length) throws CborException {
        boolean negative = majorType == MajorType.NEGATIVE_INTEGER;
        int symbol = majorType.getValue() << 5;
        if (length.compareTo(BigInteger.valueOf(24)) == -1) {
            write(symbol | length.intValue());
        } else if (length.compareTo(BigInteger.valueOf(256)) == -1) {
            symbol |= AdditionalInformation.ONE_BYTE.getValue();
            write((byte) symbol, (byte) length.intValue());
        } else if (length.compareTo(BigInteger.valueOf(65536L)) == -1) {
            symbol |= AdditionalInformation.TWO_BYTES.getValue();
            long twoByteValue = length.longValue();
            write((byte) symbol, (byte) (twoByteValue >> 8), (byte) (twoByteValue & 0xFF));
        } else if (length.compareTo(BigInteger.valueOf(4294967296L)) == -1) {
            symbol |= AdditionalInformation.FOUR_BYTES.getValue();
            long fourByteValue = length.longValue();
            write((byte) symbol, (byte) ((fourByteValue >> 24) & 0xFF), (byte) ((fourByteValue >> 16) & 0xFF),
                (byte) ((fourByteValue >> 8) & 0xFF), (byte) (fourByteValue & 0xFF));
        } else if (length.compareTo(new BigInteger("18446744073709551616")) == -1) {
            symbol |= AdditionalInformation.EIGHT_BYTES.getValue();
            BigInteger mask = BigInteger.valueOf(0xFF);
            write((byte) symbol, length.shiftRight(56).and(mask).byteValue(),
                length.shiftRight(48).and(mask).byteValue(), length.shiftRight(40).and(mask).byteValue(),
                length.shiftRight(32).and(mask).byteValue(), length.shiftRight(24).and(mask).byteValue(),
                length.shiftRight(16).and(mask).byteValue(), length.shiftRight(8).and(mask).byteValue(),
                length.and(mask).byteValue());
        } else {
            if (negative) {
                encoder.encode(new Tag(3));
            } else {
                encoder.encode(new Tag(2));
            }
            encoder.encode(new ByteString(length.toByteArray()));
        }
    }

    protected void write(int b) throws CborException {
        try {
            outputStream.write(b);
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
    }

    protected void write(byte... bytes) throws CborException {
        try {
            outputStream.write(bytes);
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
    }

}
