package co.nstant.in.cbor.decoder;

import java.io.InputStream;
import java.math.BigInteger;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.NegativeInteger;

public class NegativeIntegerDecoder extends AbstractDecoder<NegativeInteger> {

    private static final BigInteger MINUS_ONE = BigInteger.valueOf(-1);

    public NegativeIntegerDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public NegativeInteger decode(int initialByte) throws CborException {
        return new NegativeInteger(MINUS_ONE.subtract(getLengthAsBigInteger(initialByte)));
    }

}
