package co.nstant.in.cbor.decoder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.AdditionalInformation;

public abstract class AbstractDecoder<T> {

    private static final int BUFFER_SIZE = 4096;

    protected static final int INFINITY = -1;

    protected final InputStream inputStream;
    protected final CborDecoder decoder;
    private int maxPreallocationSize;

    public AbstractDecoder(CborDecoder decoder, InputStream inputStream) {
        this.decoder = decoder;
        this.inputStream = inputStream;
    }

    public abstract T decode(int initialByte) throws CborException;

    protected int nextSymbol() throws CborException {
        try {
            int symbol = inputStream.read();
            if (symbol == -1) {
                throw new IOException("Unexpected end of stream");
            }
            return symbol;
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
    }

    protected byte[] nextSymbols(int amount) throws CborException {
        try {
            byte[] symbols = new byte[amount];
            int read = inputStream.read(symbols);
            if (read == amount) {
                return symbols;
            }
            if (read == -1) {
                throw new IOException("Unexpected end of stream");
            }
            // Unlikely, but according to read contract possible:
            int left = amount - read;
            while (left > 0) {
                read = inputStream.read(symbols, amount - left, left);
                if (read == -1) {
                    throw new IOException("Unexpected end of stream");
                }
                left -= read;
            }
            return symbols;
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
    }

    byte[] decodeBytes(long length) throws CborException {
        if (length > Integer.MAX_VALUE) {
            throw new CborException("Decoding fixed size items is limited to INTMAX");
        }
        ByteArrayOutputStream bytes = new ByteArrayOutputStream(getPreallocationSize(length));
        final int chunkSize = (int) (length > BUFFER_SIZE ? BUFFER_SIZE : length);
        int i = (int) length;
        byte[] buf = new byte[chunkSize];
        while (i > 0) {
            try {
                int read = inputStream.read(buf, 0, i > chunkSize ? chunkSize : i);
                if (read == -1) {
                    throw new IOException("Unexpected end of stream");
                }
                bytes.write(buf, 0, read);
                i -= read;
            } catch (IOException e) {
                throw new CborException(e);
            }
        }
        return bytes.toByteArray();
    }

    protected long getLength(int initialByte) throws CborException {
        switch (AdditionalInformation.ofByte(initialByte)) {
        case DIRECT:
            return initialByte & 31;
        case ONE_BYTE:
            return nextSymbol();
        case TWO_BYTES:
            long twoByteValue = 0;
            byte[] symbols = nextSymbols(2);
            twoByteValue |= (symbols[0] & 0xFF) << 8;
            twoByteValue |= (symbols[1] & 0xFF) << 0;
            return twoByteValue;
        case FOUR_BYTES:
            long fourByteValue = 0L;
            symbols = nextSymbols(4);
            fourByteValue |= (long) (symbols[0] & 0xFF) << 24;
            fourByteValue |= (long) (symbols[1] & 0xFF) << 16;
            fourByteValue |= (long) (symbols[2] & 0xFF) << 8;
            fourByteValue |= (long) (symbols[3] & 0xFF) << 0;
            return fourByteValue;
        case EIGHT_BYTES:
            long eightByteValue = 0;
            symbols = nextSymbols(8);
            eightByteValue |= (long) (symbols[0] & 0xFF) << 56;
            eightByteValue |= (long) (symbols[1] & 0xFF) << 48;
            eightByteValue |= (long) (symbols[2] & 0xFF) << 40;
            eightByteValue |= (long) (symbols[3] & 0xFF) << 32;
            eightByteValue |= (long) (symbols[4] & 0xFF) << 24;
            eightByteValue |= (long) (symbols[5] & 0xFF) << 16;
            eightByteValue |= (long) (symbols[6] & 0xFF) << 8;
            eightByteValue |= (long) (symbols[7] & 0xFF) << 0;
            return eightByteValue;
        case INDEFINITE:
            return INFINITY;
        case RESERVED:
        default:
            throw new CborException("Reserved additional information");
        }
    }

    protected BigInteger getLengthAsBigInteger(int initialByte) throws CborException {
        switch (AdditionalInformation.ofByte(initialByte)) {
        case DIRECT:
            return BigInteger.valueOf(initialByte & 31);
        case ONE_BYTE:
            return BigInteger.valueOf(nextSymbol());
        case TWO_BYTES:
            long twoByteValue = 0;
            byte[] symbols = nextSymbols(2);
            twoByteValue |= (symbols[0] & 0xFF) << 8;
            twoByteValue |= (symbols[1] & 0xFF) << 0;
            return BigInteger.valueOf(twoByteValue);
        case FOUR_BYTES:
            long fourByteValue = 0L;
            symbols = nextSymbols(4);
            fourByteValue |= (long) (symbols[0] & 0xFF) << 24;
            fourByteValue |= (long) (symbols[1] & 0xFF) << 16;
            fourByteValue |= (long) (symbols[2] & 0xFF) << 8;
            fourByteValue |= (long) (symbols[3] & 0xFF) << 0;
            return BigInteger.valueOf(fourByteValue);
        case EIGHT_BYTES:
            BigInteger eightByteValue = BigInteger.ZERO;
            symbols = nextSymbols(8);
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[0] & 0xFF)).shiftLeft(56));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[1] & 0xFF)).shiftLeft(48));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[2] & 0xFF)).shiftLeft(40));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[3] & 0xFF)).shiftLeft(32));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[4] & 0xFF)).shiftLeft(24));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[5] & 0xFF)).shiftLeft(16));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[6] & 0xFF)).shiftLeft(8));
            eightByteValue = eightByteValue.or(BigInteger.valueOf((symbols[7] & 0xFF)).shiftLeft(0));
            return eightByteValue;
        case INDEFINITE:
            return BigInteger.valueOf(INFINITY);
        case RESERVED:
        default:
            throw new CborException("Reserved additional information");
        }
    }

    int getPreallocationSize(long length) {
        int len = Math.abs((int) length);
        return maxPreallocationSize > 0 ? Math.min(maxPreallocationSize, len) : len;
    }

    public void setMaxPreallocationSize(int maxPreallocationSize) {
        this.maxPreallocationSize = maxPreallocationSize;
    }
}
