package co.nstant.in.cbor.decoder;

import java.io.InputStream;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.HalfPrecisionFloat;

public class HalfPrecisionFloatDecoder extends AbstractDecoder<HalfPrecisionFloat> {

    public HalfPrecisionFloatDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public HalfPrecisionFloat decode(int initialByte) throws CborException {
        byte[] symbols = nextSymbols(2);
        int bits = (symbols[0] & 0xFF) << 8 | (symbols[1] & 0xFF);
        return new HalfPrecisionFloat(toFloat(bits));
    }

    /**
     * @see http://stackoverflow.com/a/5684578
     */
    private static float toFloat(int bits) {
        int s = (bits & 0x8000) >> 15;
        int e = (bits & 0x7C00) >> 10;
        int f = bits & 0x03FF;

        if (e == 0) {
            return (float) ((s != 0 ? -1 : 1) * Math.pow(2, -14) * (f / Math.pow(2, 10)));
        } else if (e == 0x1F) {
            return f != 0 ? Float.NaN : (s != 0 ? -1 : 1) * Float.POSITIVE_INFINITY;
        }

        return (float) ((s != 0 ? -1 : 1) * Math.pow(2, e - 15) * (1 + f / Math.pow(2, 10)));
    }

}
