package co.nstant.in.cbor.model;

import java.math.BigInteger;

import co.nstant.in.cbor.CborException;

/**
 * Rational Numbers: http://peteroupc.github.io/CBOR/rational.html
 */

public class RationalNumber extends Array {

    public RationalNumber(Number numerator, Number denominator) throws CborException {
        setTag(30);
        if (numerator == null) {
            throw new CborException("Numerator is null");
        }
        if (denominator == null) {
            throw new CborException("Denominator is null");
        }
        if (denominator.getValue().equals(BigInteger.ZERO)) {
            throw new CborException("Denominator is zero");
        }
        add(numerator);
        add(denominator);
    }

    public Number getNumerator() {
        return (Number) getDataItems().get(0);
    }

    public Number getDenominator() {
        return (Number) getDataItems().get(1);
    }

}
