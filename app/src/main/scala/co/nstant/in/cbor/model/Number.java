package co.nstant.in.cbor.model;

import java.math.BigInteger;
import java.util.Objects;

public abstract class Number extends DataItem {

    private final BigInteger value;

    protected Number(MajorType majorType, BigInteger value) {
        super(majorType);
        this.value = Objects.requireNonNull(value);
    }

    public BigInteger getValue() {
        return value;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Number) {
            Number other = (Number) object;
            return super.equals(object) && value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ value.hashCode();
    }

    @Override
    public String toString() {
        return value.toString();
    }

}
