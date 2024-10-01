package com.sparrowwallet.hummingbird;

import android.annotation.TargetApi;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.stream.IntStream;

@TargetApi(24)
public class LegacyUREncoder {
    public static final int DEFAULT_FRAGMENT_LENGTH = 200;

    private final UR ur;
    private final int fragmentLen;

    public LegacyUREncoder(UR ur) {
        this(ur, DEFAULT_FRAGMENT_LENGTH);
    }

    public LegacyUREncoder(UR ur, int fragmentLen) {
        this.ur = ur;
        this.fragmentLen = fragmentLen;
    }

    public String[] encode() {
        String encoded = BC32.encode(ur.getCborBytes());

        MessageDigest sha256Digest = newDigest();
        sha256Digest.update(ur.getCborBytes());
        byte[] checksum = sha256Digest.digest();
        String bc32Checksum = BC32.encode(checksum);

        String[] fragments = splitData(encoded, fragmentLen);
        return composeHeadersToFragments(fragments, bc32Checksum);
    }

    private String[] splitData(String s, int fragmentLen) {
        int count = (int)Math.ceil(s.length() / (float)fragmentLen);
        int partLength = (int)Math.ceil(s.length() / (float)count);
        String[] fragments = new String[count];
        for(int i = 0; i < count; i++) {
            fragments[i] = s.substring(partLength * i, Math.min(partLength * (i + 1), s.length()));
        }

        return fragments;
    }

    private String[] composeHeadersToFragments(String[] fragments, String checksum) {
        int length = fragments.length;
        if(length <= 1) {
            return Arrays.stream(fragments).map(this::composeUR).toArray(String[]::new);
        } else {
            return IntStream.range(0, length)
                    .mapToObj(i -> composeHeadersToFragment(fragments[i], checksum, i, length))
                    .toArray(String[]::new);
        }
    }

    public static MessageDigest newDigest() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);  // Can't happen.
        }
    }

    public String composeHeadersToFragment(String fragment, String checksum, int index, int total) {
        return composeUR(composeSequencing(composeChecksum(fragment, checksum), index, total));
    }

    private String composeChecksum(String payload, String checksum) {
        return String.format("%s/%s", checksum, payload);
    }

    private String composeSequencing(String payload, int index, int total) {
        return String.format("%dof%d/%s", index + 1, total, payload);
    }

    private String composeUR(String payload) {
        return composeUR(payload, ur.getType());
    }

    private String composeUR(String payload, String type) {
        return String.format("ur:%s/%s", type, payload);
    }
}
