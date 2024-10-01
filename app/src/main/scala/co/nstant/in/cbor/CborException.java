package co.nstant.in.cbor;

public class CborException extends Exception {

    private static final long serialVersionUID = 8839905301881841410L;

    public CborException(String message) {
        super(message);
    }

    public CborException(Throwable cause) {
        super(cause);
    }

    public CborException(String message, Throwable cause) {
        super(message, cause);
    }

}
