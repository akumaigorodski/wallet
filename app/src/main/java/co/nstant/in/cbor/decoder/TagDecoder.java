package co.nstant.in.cbor.decoder;

import java.io.InputStream;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.Tag;

public class TagDecoder extends AbstractDecoder<Tag> {

    public TagDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public Tag decode(int initialByte) throws CborException {
        return new Tag(getLength(initialByte));
    }

}
