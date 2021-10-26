package co.nstant.in.cbor.decoder;

import java.io.InputStream;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.SinglePrecisionFloat;

public class SinglePrecisionFloatDecoder extends AbstractDecoder<SinglePrecisionFloat> {

    public SinglePrecisionFloatDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public SinglePrecisionFloat decode(int initialByte) throws CborException {
        int bits = 0;
        byte[] symbols = nextSymbols(4);
        bits |= symbols[0] & 0xFF;
        bits <<= 8;
        bits |= symbols[1] & 0xFF;
        bits <<= 8;
        bits |= symbols[2] & 0xFF;
        bits <<= 8;
        bits |= symbols[3] & 0xFF;
        return new SinglePrecisionFloat(Float.intBitsToFloat(bits));
    }

}
