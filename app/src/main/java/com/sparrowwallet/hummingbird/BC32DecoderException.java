package com.sparrowwallet.hummingbird;

public class BC32DecoderException extends IllegalArgumentException {

    public BC32DecoderException(String message) {
        super(message);
    }

    public static class InvalidCharacter extends BC32DecoderException {
        public final char character;
        public final int position;

        public InvalidCharacter(char character, int position) {
            super("Invalid character '" + character + "' at position " + position);
            this.character = character;
            this.position = position;
        }
    }

    public static class InvalidDataLength extends BC32DecoderException {

        public InvalidDataLength(String message) {
            super(message);
        }
    }

    public static class InvalidChecksum extends BC32DecoderException {
        public InvalidChecksum() {
            super("Checksum does not validate");
        }
    }
}
