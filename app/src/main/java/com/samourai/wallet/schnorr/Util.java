package com.samourai.wallet.schnorr;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class Util  {

    public static byte[] bytesFromBigInteger(BigInteger n) {

        byte[] b = n.toByteArray();

        if(b.length == 32) {
            return b;
        }
        else if(b.length > 32) {
            return Arrays.copyOfRange(b, b.length - 32, b.length);
        }
        else {
            byte[] buf = new byte[32];
            System.arraycopy(b, 0, buf, buf.length - b.length, b.length);
            return buf;
        }
    }

    public static BigInteger bigIntFromBytes(byte[] b) {
        return new BigInteger(1, b);
    }

    public static byte[] sha256(byte[] b) throws NoSuchAlgorithmException {
        MessageDigest digest = MessageDigest.getInstance("SHA-256");
        return digest.digest(b);
    }

    public static byte[] xor(byte[] b0, byte[] b1)   {

        if(b0.length != b1.length)   {
            return  null;
        }

        byte[] ret = new byte[b0.length];
        int i = 0;
        for (byte b : b0)   {
            ret[i] = (byte)(b ^ b1[i]);
            i++;
        }

        return ret;
    }

}
