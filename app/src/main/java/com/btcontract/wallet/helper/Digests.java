package com.btcontract.wallet.helper;

import org.spongycastle.crypto.digests.RIPEMD160Digest;
import org.spongycastle.crypto.digests.SHA256Digest;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.macs.HMac;
import org.bitcoinj.core.Sha256Hash;


public final class Digests {
    static HMac createHmacSha256Digest(byte[] key) {
        SHA256Digest digest = new SHA256Digest();
        HMac hMac = new HMac(digest);
        hMac.init(new KeyParameter(key));
        return hMac;
    }

    static byte[] hmacSha256(HMac hmacSha256, byte[] data) {
        hmacSha256.reset();
        hmacSha256.update(data, 0, data.length);
        byte[] out = new byte[32];
        hmacSha256.doFinal(out, 0);
        return out;
    }

    public static byte[] hmacSha256(byte[] key, byte[] data) {
        return hmacSha256(createHmacSha256Digest(key), data);
    }

    public static byte[] ripemd160(byte[] input) {
        RIPEMD160Digest digest = new RIPEMD160Digest();
        digest.update(input, 0, input.length);
        byte[] out = new byte[20];
        digest.doFinal(out, 0);
        return out;
    }
}
