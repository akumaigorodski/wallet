package co.nstant.in.cbor.model;

/**
 * The initial byte of each data item contains both information about the major
 * type (the high-order 3 bits) and additional information (the low-order 5
 * bits). When the value of the additional information is less than 24, it is
 * directly used as a small unsigned integer. When it is 24 to 27, the
 * additional bytes for a variable-length integer immediately follow; the values
 * 24 to 27 of the additional information specify that its length is a 1-, 2-,
 * 4- or 8-byte unsigned integer, respectively. Additional information value 31
 * is used for indefinite length items, described in Section 2.2. Additional
 * information values 28 to 30 are reserved for future expansion.
 */
public enum AdditionalInformation {

    DIRECT(0), // 0-23
    ONE_BYTE(24), // 24
    TWO_BYTES(25), // 25
    FOUR_BYTES(26), // 26
    EIGHT_BYTES(27), // 27
    RESERVED(28), // 28-30
    INDEFINITE(31); // 31

    private final int value;

    private AdditionalInformation(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static AdditionalInformation ofByte(int b) {
        switch (b & 31) {
        case 24:
            return ONE_BYTE;
        case 25:
            return TWO_BYTES;
        case 26:
            return FOUR_BYTES;
        case 27:
            return EIGHT_BYTES;
        case 28:
        case 29:
        case 30:
            return RESERVED;
        case 31:
            return INDEFINITE;
        default:
            return DIRECT;
        }
    }

}
