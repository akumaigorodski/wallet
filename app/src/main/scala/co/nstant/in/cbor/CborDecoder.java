package co.nstant.in.cbor;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import co.nstant.in.cbor.decoder.ArrayDecoder;
import co.nstant.in.cbor.decoder.ByteStringDecoder;
import co.nstant.in.cbor.decoder.MapDecoder;
import co.nstant.in.cbor.decoder.NegativeIntegerDecoder;
import co.nstant.in.cbor.decoder.SpecialDecoder;
import co.nstant.in.cbor.decoder.TagDecoder;
import co.nstant.in.cbor.decoder.UnicodeStringDecoder;
import co.nstant.in.cbor.decoder.UnsignedIntegerDecoder;
import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.LanguageTaggedString;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Number;
import co.nstant.in.cbor.model.RationalNumber;
import co.nstant.in.cbor.model.Tag;
import co.nstant.in.cbor.model.UnicodeString;

/**
 * Decoder for the CBOR format based.
 */
public class CborDecoder {

    private final InputStream inputStream;
    private final UnsignedIntegerDecoder unsignedIntegerDecoder;
    private final NegativeIntegerDecoder negativeIntegerDecoder;
    private final ByteStringDecoder byteStringDecoder;
    private final UnicodeStringDecoder unicodeStringDecoder;
    private final ArrayDecoder arrayDecoder;
    private final MapDecoder mapDecoder;
    private final TagDecoder tagDecoder;
    private final SpecialDecoder specialDecoder;

    private boolean autoDecodeInfinitiveArrays = true;
    private boolean autoDecodeInfinitiveMaps = true;
    private boolean autoDecodeInfinitiveByteStrings = true;
    private boolean autoDecodeInfinitiveUnicodeStrings = true;
    private boolean autoDecodeRationalNumbers = true;
    private boolean autoDecodeLanguageTaggedStrings = true;
    private boolean rejectDuplicateKeys = false;

    /**
     * Initialize a new decoder which reads the binary encoded data from an
     * {@link OutputStream}.
     * 
     * @param inputStream the {@link OutputStream} to read the data from
     */
    public CborDecoder(InputStream inputStream) {
        Objects.requireNonNull(inputStream);
        this.inputStream = inputStream;
        unsignedIntegerDecoder = new UnsignedIntegerDecoder(this, inputStream);
        negativeIntegerDecoder = new NegativeIntegerDecoder(this, inputStream);
        byteStringDecoder = new ByteStringDecoder(this, inputStream);
        unicodeStringDecoder = new UnicodeStringDecoder(this, inputStream);
        arrayDecoder = new ArrayDecoder(this, inputStream);
        mapDecoder = new MapDecoder(this, inputStream);
        tagDecoder = new TagDecoder(this, inputStream);
        specialDecoder = new SpecialDecoder(this, inputStream);
    }

    /**
     * Convenience method to decode a byte array directly.
     *
     * @param bytes the CBOR encoded data
     * @return a list of {@link DataItem}s
     * @throws CborException if decoding failed
     */
    public static List<DataItem> decode(byte[] bytes) throws CborException {
        return new CborDecoder(new ByteArrayInputStream(bytes)).decode();
    }

    /**
     * Decode the {@link InputStream} to a list of {@link DataItem}s.
     *
     * @return the list of {@link DataItem}s
     * @throws CborException if decoding failed
     */
    public List<DataItem> decode() throws CborException {
        List<DataItem> dataItems = new LinkedList<>();
        DataItem dataItem;
        while ((dataItem = decodeNext()) != null) {
            dataItems.add(dataItem);
        }
        return dataItems;
    }

    /**
     * Streaming decoding of an input stream. On each decoded DataItem, the callback
     * listener is invoked.
     *
     * @param dataItemListener the callback listener
     * @throws CborException if decoding failed
     */
    public void decode(DataItemListener dataItemListener) throws CborException {
        Objects.requireNonNull(dataItemListener);
        DataItem dataItem = decodeNext();
        while (dataItem != null) {
            dataItemListener.onDataItem(dataItem);
            dataItem = decodeNext();
        }
    }

    /**
     * Decodes exactly one DataItem from the input stream.
     *
     * @return a {@link DataItem} or null if end of stream has reached.
     * @throws CborException if decoding failed
     */
    public DataItem decodeNext() throws CborException {
        int symbol;
        try {
            symbol = inputStream.read();
        } catch (IOException ioException) {
            throw new CborException(ioException);
        }
        if (symbol == -1) {
            return null;
        }
        switch (MajorType.ofByte(symbol)) {
        case ARRAY:
            return arrayDecoder.decode(symbol);
        case BYTE_STRING:
            return byteStringDecoder.decode(symbol);
        case MAP:
            return mapDecoder.decode(symbol);
        case NEGATIVE_INTEGER:
            return negativeIntegerDecoder.decode(symbol);
        case UNICODE_STRING:
            return unicodeStringDecoder.decode(symbol);
        case UNSIGNED_INTEGER:
            return unsignedIntegerDecoder.decode(symbol);
        case SPECIAL:
            return specialDecoder.decode(symbol);
        case TAG:
            Tag tag = tagDecoder.decode(symbol);
            DataItem next = decodeNext();
            if (next == null) {
                throw new CborException("Unexpected end of stream: tag without following data item.");
            } else {
                if (autoDecodeRationalNumbers && tag.getValue() == 30) {
                    return decodeRationalNumber(next);
                } else if (autoDecodeLanguageTaggedStrings && tag.getValue() == 38) {
                    return decodeLanguageTaggedString(next);
                } else {
                    DataItem itemToTag = next;
                    while (itemToTag.hasTag())
                        itemToTag = itemToTag.getTag();
                    itemToTag.setTag(tag);
                    return next;
                }
            }
        case INVALID:
        default:
            throw new CborException("Not implemented major type " + symbol);
        }
    }

    private DataItem decodeLanguageTaggedString(DataItem dataItem) throws CborException {
        if (!(dataItem instanceof Array)) {
            throw new CborException("Error decoding LanguageTaggedString: not an array");
        }

        Array array = (Array) dataItem;

        if (array.getDataItems().size() != 2) {
            throw new CborException("Error decoding LanguageTaggedString: array size is not 2");
        }

        DataItem languageDataItem = array.getDataItems().get(0);

        if (!(languageDataItem instanceof UnicodeString)) {
            throw new CborException("Error decoding LanguageTaggedString: first data item is not an UnicodeString");
        }

        DataItem stringDataItem = array.getDataItems().get(1);

        if (!(stringDataItem instanceof UnicodeString)) {
            throw new CborException("Error decoding LanguageTaggedString: second data item is not an UnicodeString");
        }

        UnicodeString language = (UnicodeString) languageDataItem;
        UnicodeString string = (UnicodeString) stringDataItem;

        return new LanguageTaggedString(language, string);
    }

    private DataItem decodeRationalNumber(DataItem dataItem) throws CborException {
        if (!(dataItem instanceof Array)) {
            throw new CborException("Error decoding RationalNumber: not an array");
        }

        Array array = (Array) dataItem;

        if (array.getDataItems().size() != 2) {
            throw new CborException("Error decoding RationalNumber: array size is not 2");
        }

        DataItem numeratorDataItem = array.getDataItems().get(0);

        if (!(numeratorDataItem instanceof Number)) {
            throw new CborException("Error decoding RationalNumber: first data item is not a number");
        }

        DataItem denominatorDataItem = array.getDataItems().get(1);

        if (!(denominatorDataItem instanceof Number)) {
            throw new CborException("Error decoding RationalNumber: second data item is not a number");
        }

        Number numerator = (Number) numeratorDataItem;
        Number denominator = (Number) denominatorDataItem;

        return new RationalNumber(numerator, denominator);
    }

    public boolean isAutoDecodeInfinitiveArrays() {
        return autoDecodeInfinitiveArrays;
    }

    public void setAutoDecodeInfinitiveArrays(boolean autoDecodeInfinitiveArrays) {
        this.autoDecodeInfinitiveArrays = autoDecodeInfinitiveArrays;
    }

    public boolean isAutoDecodeInfinitiveMaps() {
        return autoDecodeInfinitiveMaps;
    }

    public void setAutoDecodeInfinitiveMaps(boolean autoDecodeInfinitiveMaps) {
        this.autoDecodeInfinitiveMaps = autoDecodeInfinitiveMaps;
    }

    public boolean isAutoDecodeInfinitiveByteStrings() {
        return autoDecodeInfinitiveByteStrings;
    }

    public void setAutoDecodeInfinitiveByteStrings(boolean autoDecodeInfinitiveByteStrings) {
        this.autoDecodeInfinitiveByteStrings = autoDecodeInfinitiveByteStrings;
    }

    public boolean isAutoDecodeInfinitiveUnicodeStrings() {
        return autoDecodeInfinitiveUnicodeStrings;
    }

    public void setAutoDecodeInfinitiveUnicodeStrings(boolean autoDecodeInfinitiveUnicodeStrings) {
        this.autoDecodeInfinitiveUnicodeStrings = autoDecodeInfinitiveUnicodeStrings;
    }

    public boolean isAutoDecodeRationalNumbers() {
        return autoDecodeRationalNumbers;
    }

    public void setAutoDecodeRationalNumbers(boolean autoDecodeRationalNumbers) {
        this.autoDecodeRationalNumbers = autoDecodeRationalNumbers;
    }

    public boolean isAutoDecodeLanguageTaggedStrings() {
        return autoDecodeLanguageTaggedStrings;
    }

    public void setAutoDecodeLanguageTaggedStrings(boolean autoDecodeLanguageTaggedStrings) {
        this.autoDecodeLanguageTaggedStrings = autoDecodeLanguageTaggedStrings;
    }

    public boolean isRejectDuplicateKeys() {
        return rejectDuplicateKeys;
    }

    public void setRejectDuplicateKeys(boolean rejectDuplicateKeys) {
        this.rejectDuplicateKeys = rejectDuplicateKeys;
    }

    /**
     * Sets the given amount of bytes as maximum preallocation limit for arrays in
     * all decoders. This prevents OutOfMemory exceptions on malicious CBOR with
     * forged fixed length items. Note that items may exceed the given size when the
     * decoded data actually contains much data. This may be limited by using a
     * limiting stream.
     *
     * @param maxSize Maximum number of bytes to preallocate in array-based items.
     *                Set to 0 to disable.
     */
    public void setMaxPreallocationSize(int maxSize) {
        unsignedIntegerDecoder.setMaxPreallocationSize(maxSize);
        negativeIntegerDecoder.setMaxPreallocationSize(maxSize);
        byteStringDecoder.setMaxPreallocationSize(maxSize);
        unicodeStringDecoder.setMaxPreallocationSize(maxSize);
        arrayDecoder.setMaxPreallocationSize(maxSize);
        mapDecoder.setMaxPreallocationSize(maxSize);
        tagDecoder.setMaxPreallocationSize(maxSize);
        specialDecoder.setMaxPreallocationSize(maxSize);
    }

}
