package com.btcontract.wallet.lightning.crypto;

import org.spongycastle.crypto.generators.Poly1305KeyGenerator;
import org.spongycastle.crypto.params.ParametersWithIV;
import org.spongycastle.crypto.tls.AlertDescription;
import org.spongycastle.crypto.engines.ChaChaEngine;
import org.spongycastle.crypto.params.KeyParameter;
import org.spongycastle.crypto.tls.TlsFatalAlert;
import org.spongycastle.util.Arrays;
import java.io.IOException;


public class ChachaDecoder {
    private final ChaChaEngine decryptCipher;

    public ChachaDecoder(byte[] key, byte[] nonce) throws IOException {
        this.decryptCipher = new ChaChaEngine(20);
        this.decryptCipher.init(false, new ParametersWithIV(new KeyParameter(key), nonce));
    }

    public byte[] decodeCiphertext(byte[] additionalData, byte[] ciphertextMac)
            throws IOException {

        byte[] cipher = Arrays.copyOfRange(ciphertextMac, 0, ciphertextMac.length - 16);

        byte[] mac = Arrays.copyOfRange(ciphertextMac, ciphertextMac.length - 16, ciphertextMac.length);

        return decodeCiphertext(mac, additionalData, cipher);
    }

    public byte[] decodeCiphertext(byte[] receivedMAC, byte[] additionalData, byte[] ciphertext)
            throws IOException {

        KeyParameter macKey = initRecordMAC(decryptCipher);

        byte[] calculatedMAC = PolyKeyCreator.create(macKey, additionalData, ciphertext);

        if (!Arrays.constantTimeAreEqual(calculatedMAC, receivedMAC))
        {
            throw new TlsFatalAlert(AlertDescription.bad_record_mac);
        }

        byte[] output = new byte[ciphertext.length];
        decryptCipher.processBytes(ciphertext, 0, ciphertext.length, output, 0);

        return output;
    }

    private KeyParameter initRecordMAC(ChaChaEngine cipher)
    {
        byte[] firstBlock = new byte[64];
        cipher.processBytes(firstBlock, 0, firstBlock.length, firstBlock, 0);

        // NOTE: The BC implementation puts 'r' after 'k'
        System.arraycopy(firstBlock, 0, firstBlock, 32, 16);
        KeyParameter macKey = new KeyParameter(firstBlock, 16, 32);
        Poly1305KeyGenerator.clamp(macKey.getKey());
        return macKey;
    }

}