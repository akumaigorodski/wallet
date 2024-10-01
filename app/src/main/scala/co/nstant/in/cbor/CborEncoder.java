package co.nstant.in.cbor;

import java.io.OutputStream;
import java.util.List;
import java.util.Objects;

import co.nstant.in.cbor.encoder.ArrayEncoder;
import co.nstant.in.cbor.encoder.ByteStringEncoder;
import co.nstant.in.cbor.encoder.MapEncoder;
import co.nstant.in.cbor.encoder.NegativeIntegerEncoder;
import co.nstant.in.cbor.encoder.SpecialEncoder;
import co.nstant.in.cbor.encoder.TagEncoder;
import co.nstant.in.cbor.encoder.UnicodeStringEncoder;
import co.nstant.in.cbor.encoder.UnsignedIntegerEncoder;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.NegativeInteger;
import co.nstant.in.cbor.model.SimpleValue;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.Tag;
import co.nstant.in.cbor.model.UnicodeString;
import co.nstant.in.cbor.model.UnsignedInteger;

/**
 * Encoder for the CBOR format based.
 */
public class CborEncoder {

    private final UnsignedIntegerEncoder unsignedIntegerEncoder;
    private final NegativeIntegerEncoder negativeIntegerEncoder;
    private final ByteStringEncoder byteStringEncoder;
    private final UnicodeStringEncoder unicodeStringEncoder;
    private final ArrayEncoder arrayEncoder;
    private final MapEncoder mapEncoder;
    private final TagEncoder tagEncoder;
    private final SpecialEncoder specialEncoder;
    private boolean canonical = true;

    /**
     * Initialize a new encoder which writes the binary encoded data to an
     * {@link OutputStream}.
     * 
     * @param outputStream the {@link OutputStream} to write the encoded data to
     */
    public CborEncoder(OutputStream outputStream) {
        Objects.requireNonNull(outputStream);
        unsignedIntegerEncoder = new UnsignedIntegerEncoder(this, outputStream);
        negativeIntegerEncoder = new NegativeIntegerEncoder(this, outputStream);
        byteStringEncoder = new ByteStringEncoder(this, outputStream);
        unicodeStringEncoder = new UnicodeStringEncoder(this, outputStream);
        arrayEncoder = new ArrayEncoder(this, outputStream);
        mapEncoder = new MapEncoder(this, outputStream);
        tagEncoder = new TagEncoder(this, outputStream);
        specialEncoder = new SpecialEncoder(this, outputStream);
    }

    /**
     * Encode a list of {@link DataItem}s, also known as a stream.
     *
     * @param dataItems a list of {@link DataItem}s
     * @throws CborException if the {@link DataItem}s could not be encoded or there
     *                       was an problem with the {@link OutputStream}.
     */
    public void encode(List<DataItem> dataItems) throws CborException {
        for (DataItem dataItem : dataItems) {
            encode(dataItem);
        }
    }

    /**
     * Encode a single {@link DataItem}.
     *
     * @param dataItem the {@link DataItem} to encode. If null, encoder encodes a
     *                 {@link SimpleValue} NULL value.
     * @throws CborException if {@link DataItem} could not be encoded or there was
     *                       an problem with the {@link OutputStream}.
     */
    public void encode(DataItem dataItem) throws CborException {
        if (dataItem == null) {
            dataItem = SimpleValue.NULL;
        }

        if (dataItem.hasTag()) {
            Tag tagDi = dataItem.getTag();
            encode(tagDi);
        }

        switch (dataItem.getMajorType()) {
        case UNSIGNED_INTEGER:
            unsignedIntegerEncoder.encode((UnsignedInteger) dataItem);
            break;
        case NEGATIVE_INTEGER:
            negativeIntegerEncoder.encode((NegativeInteger) dataItem);
            break;
        case BYTE_STRING:
            byteStringEncoder.encode((ByteString) dataItem);
            break;
        case UNICODE_STRING:
            unicodeStringEncoder.encode((UnicodeString) dataItem);
            break;
        case ARRAY:
            arrayEncoder.encode((Array) dataItem);
            break;
        case MAP:
            mapEncoder.encode((Map) dataItem);
            break;
        case SPECIAL:
            specialEncoder.encode((Special) dataItem);
            break;
        case TAG:
            tagEncoder.encode((Tag) dataItem);
            break;
        default:
            throw new CborException("Unknown major type");
        }
    }

    public boolean isCanonical() {
        return canonical;
    }

    public CborEncoder nonCanonical() {
        canonical = false;
        return this;
    }

}
