package com.btcontract.wallet.helper;

import org.spongycastle.crypto.digests.SHA256Digest;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.macs.HMac;


public final class HMacSha256 {
    static HMac createHmacSha256Digest(byte[] key) {
        SHA256Digest digest = new SHA256Digest();
        HMac hMac = new HMac(digest);
        hMac.init(new KeyParameter(key));
        return hMac;
    }

    static byte[] hmacSha256(HMac hmacSha256, byte[] input) {
        hmacSha256.reset();
        hmacSha256.update(input, 0, input.length);
        byte[] out = new byte[32];
        hmacSha256.doFinal(out, 0);
        return out;
    }

    public static byte[] make(byte[] data, byte[] key) {
        return hmacSha256(createHmacSha256Digest(key), data);
    }
}
