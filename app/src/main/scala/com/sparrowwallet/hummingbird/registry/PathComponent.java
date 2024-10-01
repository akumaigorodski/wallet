package com.sparrowwallet.hummingbird.registry;

public class PathComponent {
    public static final int HARDENED_BIT = 0x80000000;

    private final int index;
    private final boolean wildcard;
    private final boolean hardened;

    public PathComponent(int index, boolean hardened) {
        this.index = index;
        this.wildcard = false;
        this.hardened = hardened;

        if((index & HARDENED_BIT) != 0) {
            throw new IllegalArgumentException("Invalid index " + index + " - most significant bit cannot be set");
        }
    }

    public PathComponent(boolean hardened) {
        this.index = 0;
        this.wildcard = true;
        this.hardened = hardened;
    }

    public int getIndex() {
        return index;
    }

    public boolean isWildcard() {
        return wildcard;
    }

    public boolean isHardened() {
        return hardened;
    }
}
