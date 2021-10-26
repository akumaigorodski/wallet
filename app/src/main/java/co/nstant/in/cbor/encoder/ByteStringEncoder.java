package co.nstant.in.cbor.encoder;

import java.io.OutputStream;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.ByteString;
import co.nstant.in.cbor.model.MajorType;
import co.nstant.in.cbor.model.SimpleValue;

public class ByteStringEncoder extends AbstractEncoder<ByteString> {

    public ByteStringEncoder(CborEncoder encoder, OutputStream outputStream) {
        super(encoder, outputStream);
    }

    @Override
    public void encode(ByteString byteString) throws CborException {
        byte[] bytes = byteString.getBytes();
        if (byteString.isChunked()) {
            encodeTypeChunked(MajorType.BYTE_STRING);
            if (bytes != null) {
                encode(new ByteString(bytes));
            }
        } else if (bytes == null) {
            encoder.encode(SimpleValue.NULL);
        } else {
            encodeTypeAndLength(MajorType.BYTE_STRING, bytes.length);
            write(bytes);
        }
    }

}
