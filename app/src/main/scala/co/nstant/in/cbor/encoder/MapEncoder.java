package co.nstant.in.cbor.encoder;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Comparator;
import java.util.TreeMap;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.SimpleValue;

public class MapEncoder extends AbstractEncoder<Map> {

    public MapEncoder(CborEncoder encoder, OutputStream outputStream) {
        super(encoder, outputStream);
    }

    @Override
    public void encode(Map map) throws CborException {
        Collection<DataItem> keys = map.getKeys();

        if (map.isChunked()) {
            encodeTypeChunked(MajorType.MAP);
        } else {
            encodeTypeAndLength(MajorType.MAP, keys.size());
        }

        if (keys.isEmpty()) {
            return;
        }

        if (map.isChunked()) {
            encodeNonCanonical(map);
            encoder.encode(SimpleValue.BREAK);
        } else {
            if (encoder.isCanonical()) {
                encodeCanonical(map);
            } else {
                encodeNonCanonical(map);
            }
        }
    }

    private void encodeNonCanonical(Map map) throws CborException {
        for (DataItem key : map.getKeys()) {
            encoder.encode(key);
            encoder.encode(map.get(key));
        }
    }

    private void encodeCanonical(Map map) throws CborException {
        /**
         * From https://tools.ietf.org/html/rfc7049#section-3.9
         * 
         * Canonical CBOR
         * 
         * The keys in every map must be sorted lowest value to highest. Sorting is
         * performed on the bytes of the representation of the key data items without
         * paying attention to the 3/5 bit splitting for major types. (Note that this
         * rule allows maps that have keys of different types, even though that is
         * probably a bad practice that could lead to errors in some canonicalization
         * implementations.) The sorting rules are:
         * 
         * If two keys have different lengths, the shorter one sorts earlier;
         * 
         * If two keys have the same length, the one with the lower value in (byte-wise)
         * lexical order sorts earlier.
         */

        TreeMap<byte[], byte[]> sortedMap = new TreeMap<>(new Comparator<byte[]>() {

            @Override
            public int compare(byte[] o1, byte[] o2) {
                if (o1.length < o2.length) {
                    return -1;
                }
                if (o1.length > o2.length) {
                    return 1;
                }
                for (int i = 0; i < o1.length; i++) {
                    if (o1[i] < o2[i]) {
                        return -1;
                    }
                    if (o1[i] > o2[i]) {
                        return 1;
                    }
                }
                return 0;
            }

        });

        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        CborEncoder e = new CborEncoder(byteArrayOutputStream);
        for (DataItem key : map.getKeys()) {
            // Key
            e.encode(key);
            byte[] keyBytes = byteArrayOutputStream.toByteArray();
            byteArrayOutputStream.reset();
            // Value
            e.encode(map.get(key));
            byte[] valueBytes = byteArrayOutputStream.toByteArray();
            byteArrayOutputStream.reset();
            sortedMap.put(keyBytes, valueBytes);
        }
        for (java.util.Map.Entry<byte[], byte[]> entry : sortedMap.entrySet()) {
            write(entry.getKey());
            write(entry.getValue());
        }
    }

}
