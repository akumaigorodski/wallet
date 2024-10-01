package co.nstant.in.cbor.model;

import java.util.Objects;

public class AbstractFloat extends Special {

    private final float value;

    public AbstractFloat(SpecialType specialType, float value) {
        super(specialType);
        this.value = value;
    }

    public float getValue() {
        return value;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof AbstractFloat) {
            AbstractFloat other = (AbstractFloat) object;
            return super.equals(object) && value == other.value;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ Objects.hashCode(value);
    }

}
