package co.nstant.in.cbor.model;

public class UnicodeString extends ChunkableDataItem {

    private final String string;

    public UnicodeString(String string) {
        super(MajorType.UNICODE_STRING);
        this.string = string;
    }

    @Override
    public String toString() {
        if (string == null) {
            return "null";
        } else {
            return string;
        }
    }

    public String getString() {
        return string;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof UnicodeString && super.equals(object)) {
            UnicodeString other = (UnicodeString) object;
            if (string == null) {
                return other.string == null;
            } else {
                return string.equals(other.string);
            }
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 0;

        if (string != null) {
            hash = super.hashCode();
            hash += string.hashCode();
        }

        return hash;
    }

}
