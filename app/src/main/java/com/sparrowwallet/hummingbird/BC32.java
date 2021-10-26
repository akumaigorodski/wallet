package com.sparrowwallet.hummingbird;

/*
 * Copyright 2018 Coinomi Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BC32 {
    /**
     * The BC32 character set for encoding.
     */
    private static final String CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l";

    /**
     *
     * Find the polynomial with value coefficients mod the generator as 30-bit.
     */
    private static int polymod(final byte[] values) {
        int c = 1;
        for (byte v_i : values) {
            int c0 = (c >>> 25) & 0xff;
            c = ((c & 0x1ffffff) << 5) ^ (v_i & 0xff);
            if ((c0 & 1) != 0) c ^= 0x3b6a57b2;
            if ((c0 & 2) != 0) c ^= 0x26508e6d;
            if ((c0 & 4) != 0) c ^= 0x1ea119fa;
            if ((c0 & 8) != 0) c ^= 0x3d4233dd;
            if ((c0 & 16) != 0) c ^= 0x2a1462b3;
        }
        return c;
    }

    /**
     * Verify a checksum.
     */
    private static boolean verifyChecksum(final byte[] values) {
        byte[] combined = new byte[1 + values.length];
        System.arraycopy(new byte[]{0}, 0, combined, 0, 1);
        System.arraycopy(values, 0, combined, 1, values.length);
        return polymod(combined) == 0x3fffffff;
    }

    /**
     * Create a checksum.
     */
    private static byte[] createChecksum(final byte[] values) {
        byte[] enc = new byte[1 + values.length + 6];
        System.arraycopy(values, 0, enc, 1, values.length);
        int mod = polymod(enc) ^ 0x3fffffff;
        byte[] result = new byte[6];
        for (int i = 0; i < 6; ++i) {
            result[i] = (byte) ((mod >>> (5 * (5 - i))) & 31);
        }
        return result;
    }

    /**
     * Encode a BC32 string.
     *
     * @param values the bytes to encode
     * @return the encoded BC32 string
     */
    public static String encode(final byte[] values) {
        List<Byte> boxedList = new ArrayList<>(values.length);
        for(byte value : values) {
            boxedList.add(value);
        }

        byte[] data = convertBits(boxedList, 8, 5, true);

        byte[] checksum = createChecksum(data);
        byte[] combined = new byte[data.length + checksum.length];
        System.arraycopy(data, 0, combined, 0, data.length);
        System.arraycopy(checksum, 0, combined, data.length, checksum.length);

        StringBuilder sb = new StringBuilder(combined.length);
        for (byte b : combined) {
            sb.append(CHARSET.charAt(b));
        }
        return sb.toString();
    }

    /**
     * Decode a BC32 string.
     *
     * @param str the String to decode
     * @return the decoded bytes
     */
    public static byte[] decode(final String str) {
        boolean lower = false, upper = false;
        if (str.length() < 6)
            throw new BC32DecoderException.InvalidDataLength("Input too short: " + str.length());
        for (int i = 0; i < str.length(); ++i) {
            char c = str.charAt(i);
            if (c < 33 || c > 126) throw new BC32DecoderException.InvalidCharacter(c, i);
            if (c >= 'a' && c <= 'z') {
                if (upper)
                    throw new BC32DecoderException.InvalidCharacter(c, i);
                lower = true;
            }
            if (c >= 'A' && c <= 'Z') {
                if (lower)
                    throw new BC32DecoderException.InvalidCharacter(c, i);
                upper = true;
            }
        }

        final int dataPartLength = str.length();
        byte[] values = new byte[dataPartLength];
        for (int i = 0; i < dataPartLength; ++i) {
            char c = str.charAt(i);

            if(CHARSET.indexOf(c) == -1)    {
                throw new IllegalArgumentException("BC32 characters  out of range");
            }
            values[i] = (byte) CHARSET.indexOf(c);
        }

        if (!verifyChecksum(values)) throw new BC32DecoderException.InvalidChecksum();

        byte[] data = Arrays.copyOfRange(values, 0, values.length - 6);
        List<Byte> valueList = new ArrayList<>();
        for (byte b : data) {
            valueList.add(b);
        }
        return convertBits(valueList, 5, 8, false);
    }

    private static byte[] convertBits(List<Byte> data, int fromBits, int toBits, boolean pad) {
        int acc = 0;
        int bits = 0;
        int maxv = (1 << toBits) - 1;
        List<Byte> ret = new ArrayList<>();

        for (Byte value : data) {
            short b = (short) (value & 0xff);
            if ((b >> fromBits) > 0) {
                throw new IllegalArgumentException("Illegal bytes for bc32 encoding");
            }

            acc = (acc << fromBits) | b;
            bits += fromBits;
            while (bits >= toBits) {
                bits -= toBits;
                ret.add((byte) ((acc >> bits) & maxv));
            }
        }

        if (pad && (bits > 0)) {
            ret.add((byte) ((acc << (toBits - bits)) & maxv));
        } else if (bits >= fromBits || (byte) (((acc << (toBits - bits)) & maxv)) != 0) {
            throw new IllegalArgumentException("Illegal bytes for bc32 encoding");
        }

        Object[] boxedArray = ret.toArray();
        int len = boxedArray.length;
        byte[] array = new byte[len];
        for (int i = 0; i < len; i++) {
            array[i] = ((Number)boxedArray[i]).byteValue();
        }

        return array;
    }
}