package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.*;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class CryptoAccount extends RegistryItem {
    public static final long MASTER_FINGERPRINT_KEY = 1;
    public static final long OUTPUT_DESCRIPTORS_KEY = 2;

    private final byte[] masterFingerprint;
    private final List<CryptoOutput> outputDescriptors;

    public CryptoAccount(byte[] masterFingerprint, List<CryptoOutput> outputDescriptors) {
        this.masterFingerprint = Arrays.copyOfRange(masterFingerprint, masterFingerprint.length - 4, masterFingerprint.length);
        this.outputDescriptors = outputDescriptors;
    }

    public byte[] getMasterFingerprint() {
        return masterFingerprint;
    }

    public List<CryptoOutput> getOutputDescriptors() {
        return outputDescriptors;
    }

    public DataItem toCbor() {
        Map map = new Map();
        map.put(new UnsignedInteger(MASTER_FINGERPRINT_KEY), new UnsignedInteger(new BigInteger(1, masterFingerprint)));
        Array array = new Array();
        for(CryptoOutput cryptoOutput : outputDescriptors) {
            array.add(cryptoOutput.toCbor());
        }
        map.put(new UnsignedInteger(OUTPUT_DESCRIPTORS_KEY), array);
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_ACCOUNT;
    }

    public static CryptoAccount fromCbor(DataItem cbor) {
        Map cryptoAccountMap = (Map)cbor;

        UnsignedInteger uintMasterFingerprint = (UnsignedInteger)cryptoAccountMap.get(new UnsignedInteger(MASTER_FINGERPRINT_KEY));
        byte[] masterFingerprint = bigIntegerToBytes(uintMasterFingerprint.getValue(), 4);
        Array outputDescriptors = (Array)cryptoAccountMap.get(new UnsignedInteger(OUTPUT_DESCRIPTORS_KEY));
        List<CryptoOutput> cryptoOutputs = new ArrayList<>(outputDescriptors.getDataItems().size());
        for(DataItem item : outputDescriptors.getDataItems()) {
            cryptoOutputs.add(CryptoOutput.fromCbor(item));
        }

        return new CryptoAccount(masterFingerprint, cryptoOutputs);
    }
}
