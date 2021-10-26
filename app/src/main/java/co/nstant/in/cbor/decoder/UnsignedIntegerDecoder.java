package co.nstant.in.cbor.decoder;

import java.io.InputStream;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.UnsignedInteger;

public class UnsignedIntegerDecoder extends AbstractDecoder<UnsignedInteger> {

    public UnsignedIntegerDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public UnsignedInteger decode(int initialByte) throws CborException {
        return new UnsignedInteger(getLengthAsBigInteger(initialByte));
    }

}
