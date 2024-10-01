package co.nstant.in.cbor.model;

import java.util.Objects;

public class DoublePrecisionFloat extends Special {

    private final double value;

    public DoublePrecisionFloat(double value) {
        super(SpecialType.IEEE_754_DOUBLE_PRECISION_FLOAT);
        this.value = value;
    }

    public double getValue() {
        return value;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof DoublePrecisionFloat) {
            DoublePrecisionFloat other = (DoublePrecisionFloat) object;
            return super.equals(object) && value == other.value;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ Objects.hashCode(value);
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

}
