package co.nstant.in.cbor.model;

import java.util.Objects;

public class Special extends DataItem {

    public static final Special BREAK = new Special(SpecialType.BREAK);

    private final SpecialType specialType;

    protected Special(SpecialType specialType) {
        super(MajorType.SPECIAL);
        this.specialType = Objects.requireNonNull(specialType);
    }

    public SpecialType getSpecialType() {
        return specialType;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Special) {
            Special other = (Special) object;
            return super.equals(object) && specialType == other.specialType;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ Objects.hashCode(specialType);
    }

    @Override
    public String toString() {
        return specialType.name();
    }

}
