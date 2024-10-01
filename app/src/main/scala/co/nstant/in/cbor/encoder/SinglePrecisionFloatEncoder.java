package co.nstant.in.cbor.encoder;

import java.io.OutputStream;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.SinglePrecisionFloat;

public class SinglePrecisionFloatEncoder extends AbstractEncoder<SinglePrecisionFloat> {

    public SinglePrecisionFloatEncoder(CborEncoder encoder, OutputStream outputStream) {
        super(encoder, outputStream);
    }

    @Override
    public void encode(SinglePrecisionFloat dataItem) throws CborException {
        int bits = Float.floatToRawIntBits(dataItem.getValue());
        write((byte) ((7 << 5) | 26), (byte) ((bits >> 24) & 0xFF), (byte) ((bits >> 16) & 0xFF),
            (byte) ((bits >> 8) & 0xFF), (byte) ((bits >> 0) & 0xFF));
    }

}
