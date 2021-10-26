package com.sparrowwallet.hummingbird;

import android.annotation.TargetApi;

import java.security.MessageDigest;
import java.util.*;

@TargetApi(24)
public class LegacyURDecoder {
    private final Set<String> fragments = new LinkedHashSet<>();

    public void receivePart(String fragment) {
        fragments.add(fragment.toLowerCase());
    }

    public boolean isComplete() {
        if(fragments.isEmpty()) {
            return false;
        }

        String fragment = fragments.iterator().next();
        String[] components = fragment.split("/");
        if(components.length > 3) {
            int[] sequence = checkAndGetSequence(components[1]);
            int total = sequence[1];
            return total == fragments.size();
        }

        return true;
    }

    public double getPercentComplete() {
        if(fragments.isEmpty()) {
            return 0d;
        }

        String fragment = fragments.iterator().next();
        String[] components = fragment.split("/");
        if(components.length > 3) {
            int[] sequence = checkAndGetSequence(components[1]);
            int total = sequence[1];
            if(total > 0 && fragments.size() <= total) {
                return (double)fragments.size() / total;
            }
        }

        return 1d;
    }

    public static boolean isLegacyURFragment(String fragment) {
        String[] components = fragment.split("/");

        //Multi-part legacy encoding may include digest which adds an extra component
        if(components.length > 3) {
            return true;
        }

        //Last component is always fragment payload in both legacy and current
        String payload = components[components.length-1].toLowerCase();

        //BC32 will never contain the following characters
        if(payload.indexOf('b') > -1 || payload.indexOf('i') > -1 || payload.indexOf('o') > -1) {
            return false;
        }
        
        //Bytewords and BC32 strings can usually be distinguished because the latter includes digits as permissible characters
        return (payload.matches(".*\\d.*"));
    }

    public UR decode() throws UR.InvalidTypeException {
        return decode(fragments.toArray(new String[0]));
    }

    public static UR decode(String[] fragments) throws UR.InvalidTypeException {
        int length = fragments.length;
        if(length == 1){
            return handleFragment(fragments[0]);
        }
        else {
            return handleFragments(fragments);
        }
    }

    private static UR handleFragment(String fragment) throws UR.InvalidTypeException {
        String[] components = fragment.split("/");

        switch(components.length) {
            case 2:
                return new UR(components[0].substring(UR.UR_PREFIX.length() + 1), BC32.decode(components[1]));
            case 3:
                String digest = components[1];
                String data = components[2];
                checkDigest(data, digest);
                return new UR(components[0].substring(UR.UR_PREFIX.length() + 1), BC32.decode(data));
            case 4:
                checkAndGetSequence(components[1]);
                String seqDigest = components[2];
                String seqData = components[3];
                checkDigest(seqDigest, fragment);
                return new UR(components[0].substring(UR.UR_PREFIX.length() + 1), BC32.decode(seqData));
            default:
                throw new IllegalArgumentException("Invalid number of fragments: expected 2 / 3 / 4 but got " + components.length);
        }
    }

    private static UR handleFragments(String[] fragments) throws UR.InvalidTypeException {
        int length = fragments.length;
        String[] parts = new String[length];
        Arrays.fill(parts, "");
        String type = null;
        String digest = null;
        for(String fragment : fragments) {
            String[] components = fragment.split("/");
            if(components.length < 4) {
                throw new IllegalArgumentException(String.format("Invalid fragment: %s, insufficient number of components (%d)", fragment, components.length));
            }
            if(type != null && !type.equals(components[0])) {
                throw new IllegalArgumentException(String.format("Invalid fragment: %s, checksum changed %s, %s", fragment, type, components[0]));
            }
            type = components[0];
            int[] sequence = checkAndGetSequence(components[1]);
            int index = sequence[0];
            int total = sequence[1];
            if(total != length) {
                throw new IllegalArgumentException(String.format("Invalid fragment: %s, total %d not equals to fragments length %d", fragment, total, length));
            }
            if(digest != null && !digest.equals(components[2])) {
                throw new IllegalArgumentException(String.format("Invalid fragment: %s, checksum changed %s, %s", fragment, digest, components[2]));
            }
            digest = components[2];
            if(parts[index - 1].length() > 0) {
                throw new IllegalArgumentException(String.format("Invalid fragment: %s, index %d has already been set", fragment, index));
            }
            parts[index - 1] = components[3];
        }
        String payload = Arrays.stream(parts).reduce((cur, acc) -> cur+acc).orElse("");
        checkDigest(payload, digest);

        if(type == null) {
            throw new IllegalStateException("Type is null");
        }

        return new UR(type.substring(UR.UR_PREFIX.length() + 1), BC32.decode(payload));
    }

    private static void checkDigest(String payload, String digest) {
        MessageDigest sha256Digest = LegacyUREncoder.newDigest();
        sha256Digest.update(BC32.decode(payload));
        byte[] calculatedChecksum = sha256Digest.digest();
        byte[] checksum = BC32.decode(digest);

        if(!Arrays.equals(calculatedChecksum, checksum)) {
            throw new IllegalArgumentException("Invalid digest: " + digest + " for payload: " + payload);
        }
    }

    public static int[] checkAndGetSequence(String payload) {
        String[] pieces = payload.toLowerCase().split("of");
        if(pieces.length != 2) {
            throw new IllegalArgumentException("Invalid sequence: " + payload);
        }
        int index = Integer.parseInt(pieces[0]);
        int total = Integer.parseInt(pieces[1]);
        if(index < 1 || index > total) {
            throw new IllegalArgumentException("Invalid sequence: " + payload);
        }
        return new int[]{index, total};
    }
}
