package co.nstant.in.cbor.decoder;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import co.nstant.in.cbor.CborDecoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.Special;
import co.nstant.in.cbor.model.UnicodeString;

public class UnicodeStringDecoder extends AbstractDecoder<UnicodeString> {

    public UnicodeStringDecoder(CborDecoder decoder, InputStream inputStream) {
        super(decoder, inputStream);
    }

    @Override
    public UnicodeString decode(int initialByte) throws CborException {
        long length = getLength(initialByte);
        if (length == INFINITY) {
            if (decoder.isAutoDecodeInfinitiveUnicodeStrings()) {
                return decodeInfinitiveLength();
            } else {
                UnicodeString unicodeString = new UnicodeString(null);
                unicodeString.setChunked(true);
                return unicodeString;
            }
        } else {
            return decodeFixedLength(length);
        }
    }

    private UnicodeString decodeInfinitiveLength() throws CborException {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        for (;;) {
            DataItem dataItem = decoder.decodeNext();
            if (dataItem == null) {
                throw new CborException("Unexpected end of stream");
            }
            MajorType majorType = dataItem.getMajorType();
            if (Special.BREAK.equals(dataItem)) {
                break;
            } else if (majorType == MajorType.UNICODE_STRING) {
                UnicodeString unicodeString = (UnicodeString) dataItem;
                byte[] byteArray = unicodeString.toString().getBytes(StandardCharsets.UTF_8);
                bytes.write(byteArray, 0, byteArray.length);
            } else {
                throw new CborException("Unexpected major type " + majorType);
            }
        }
        return new UnicodeString(new String(bytes.toByteArray(), StandardCharsets.UTF_8));
    }

    private UnicodeString decodeFixedLength(long length) throws CborException {
        return new UnicodeString(new String(decodeBytes(length), StandardCharsets.UTF_8));
    }

}
