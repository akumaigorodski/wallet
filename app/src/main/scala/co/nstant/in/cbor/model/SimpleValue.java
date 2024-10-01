package co.nstant.in.cbor.model;

import java.util.Objects;

public class SimpleValue extends Special {

    private final SimpleValueType simpleValueType;

    public static final SimpleValue FALSE = new SimpleValue(SimpleValueType.FALSE);
    public static final SimpleValue TRUE = new SimpleValue(SimpleValueType.TRUE);
    public static final SimpleValue NULL = new SimpleValue(SimpleValueType.NULL);
    public static final SimpleValue UNDEFINED = new SimpleValue(SimpleValueType.UNDEFINED);

    private final int value;

    public SimpleValue(SimpleValueType simpleValueType) {
        super(SpecialType.SIMPLE_VALUE);
        this.value = simpleValueType.getValue();
        this.simpleValueType = simpleValueType;
    }

    public SimpleValue(int value) {
        super(value <= 23 ? SpecialType.SIMPLE_VALUE : SpecialType.SIMPLE_VALUE_NEXT_BYTE);
        this.value = value;
        this.simpleValueType = SimpleValueType.ofByte(value);
    }

    public SimpleValueType getSimpleValueType() {
        return simpleValueType;
    }

    public int getValue() {
        return value;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof SimpleValue) {
            SimpleValue other = (SimpleValue) object;
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
        return simpleValueType.toString();
    }

}
