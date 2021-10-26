package co.nstant.in.cbor.decoder;

import java.io.InputStream;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.Special;

public class MapDecoder extends AbstractDecoder<Map> {

    public MapDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public Map decode(int initialByte) throws CborException {
        long length = getLength(initialByte);
        if (length == INFINITY) {
            return decodeInfinitiveLength();
        } else {
            return decodeFixedLength(length);
        }
    }

    private Map decodeInfinitiveLength() throws CborException {
        Map map = new Map();
        map.setChunked(true);
        if (decoder.isAutoDecodeInfinitiveMaps()) {
            for (;;) {
                DataItem key = decoder.decodeNext();
                if (Special.BREAK.equals(key)) {
                    break;
                }
                DataItem value = decoder.decodeNext();
                if (key == null || value == null) {
                    throw new CborException("Unexpected end of stream");
                }
                if (decoder.isRejectDuplicateKeys() && map.get(key) != null) {
                    throw new CborException("Duplicate key found in map");
                }
                map.put(key, value);
            }
        }
        return map;
    }

    private Map decodeFixedLength(long length) throws CborException {
        Map map = new Map(getPreallocationSize(length));
        for (long i = 0; i < length; i++) {
            DataItem key = decoder.decodeNext();
            DataItem value = decoder.decodeNext();
            if (key == null || value == null) {
                throw new CborException("Unexpected end of stream");
            }
            if (decoder.isRejectDuplicateKeys() && map.get(key) != null) {
                throw new CborException("Duplicate key found in map");
            }
            map.put(key, value);
        }
        return map;
    }

}
