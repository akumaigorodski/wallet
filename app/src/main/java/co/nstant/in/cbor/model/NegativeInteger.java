package co.nstant.in.cbor.model;

import java.math.BigInteger;

public class NegativeInteger extends Number {

    public NegativeInteger(long value) {
        this(BigInteger.valueOf(value));
        assertTrue(value < 0L, "value " + value + " is not < 0");
    }

    public NegativeInteger(BigInteger value) {
        super(MajorType.NEGATIVE_INTEGER, value);
    }

}
