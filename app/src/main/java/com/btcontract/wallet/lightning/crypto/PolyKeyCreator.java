package com.btcontract.wallet.lightning.crypto;

import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.macs.Poly1305;
import org.spongycastle.util.Pack;


class PolyKeyCreator {
    public static byte[] create(KeyParameter macKey, byte[] additionalData, byte[] ciphertext) {
        Poly1305 poly = new Poly1305();
        poly.init(macKey);

        if (additionalData != null) {
            poly.update(additionalData, 0, additionalData.length);
            if (additionalData.length % 16 != 0) {
                int round = 16 - (additionalData.length % 16);
                poly.update(new byte[round], 0, round);
            }
        }

        poly.update(ciphertext, 0, ciphertext.length);
        if (ciphertext.length % 16 != 0) {
            int round = 16 - (ciphertext.length % 16);
            poly.update(new byte[round], 0, round);
        }

        //additional data length
        byte[] additionalDataLength;
        if (additionalData != null) {
            additionalDataLength = Pack.longToLittleEndian(additionalData.length);
        } else {
            additionalDataLength = new byte[8];
        }

        poly.update(additionalDataLength, 0, 8);
        byte[] ciphertextLength = Pack.longToLittleEndian(ciphertext.length);
        poly.update(ciphertextLength, 0, 8);

        byte[] calculatedMAC = new byte[poly.getMacSize()];
        poly.doFinal(calculatedMAC, 0);
        return calculatedMAC;
    }
}