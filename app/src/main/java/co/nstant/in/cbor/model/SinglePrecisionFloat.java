package co.nstant.in.cbor.model;

public class SinglePrecisionFloat extends AbstractFloat {

    public SinglePrecisionFloat(float value) {
        super(SpecialType.IEEE_754_SINGLE_PRECISION_FLOAT, value);
    }

}
