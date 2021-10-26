package co.nstant.in.cbor.encoder;

import java.io.OutputStream;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.HalfPrecisionFloat;

public class HalfPrecisionFloatEncoder extends AbstractEncoder<HalfPrecisionFloat> {

    public HalfPrecisionFloatEncoder(CborEncoder encoder, OutputStream outputStream) {
        super(encoder, outputStream);
    }

    @Override
    public void encode(HalfPrecisionFloat dataItem) throws CborException {
        int bits = fromFloat(dataItem.getValue());
        write((byte) ((7 << 5) | 25), (byte) ((bits >> 8) & 0xFF), (byte) ((bits >> 0) & 0xFF));
    }

    /**
     * @param fval the float value
     * @return all higher 16 bits as 0 for all results
     * @see <a href="http://stackoverflow.com/a/6162687">Stack Overflow</a>
     */
    public static int fromFloat(float fval) {
        int fbits = Float.floatToIntBits(fval);
        int sign = fbits >>> 16 & 0x8000; // sign only
        int val = 0x1000 + fbits & 0x7fffffff; // rounded value

        if (val >= 0x47800000) // might be or become NaN/Inf
        { // avoid Inf due to rounding
            if ((fbits & 0x7fffffff) >= 0x47800000) { // is or must become
                                                      // NaN/Inf
                if (val < 0x7f800000) {// was value but too large
                    return sign | 0x7c00; // make it +/-Inf
                }
                return sign | 0x7c00 | // remains +/-Inf or NaN
                    (fbits & 0x007fffff) >>> 13; // keep NaN (and
                                                 // Inf) bits
            }
            return sign | 0x7bff; // unrounded not quite Inf
        }
        if (val >= 0x38800000) { // remains normalized value
            return sign | val - 0x38000000 >>> 13; // exp - 127 + 15
        }
        if (val < 0x33000000) { // too small for subnormal
            return sign; // becomes +/-0
        }
        val = (fbits & 0x7fffffff) >>> 23; // tmp exp for subnormal calc
        return sign | ((fbits & 0x7fffff | 0x800000) // add subnormal bit
            + (0x800000 >>> val - 102) // round depending on cut off
            >>> 126 - val); // div by 2^(1-(exp-127+15)) and >> 13 | exp=0
    }

}
