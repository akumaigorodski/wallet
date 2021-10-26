package co.nstant.in.cbor.model;

public class HalfPrecisionFloat extends AbstractFloat {

    public HalfPrecisionFloat(float value) {
        super(SpecialType.IEEE_754_HALF_PRECISION_FLOAT, value);
    }

}
