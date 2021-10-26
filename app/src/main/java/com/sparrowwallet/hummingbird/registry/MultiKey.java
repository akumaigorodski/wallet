package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnsignedInteger;

import java.util.ArrayList;
import java.util.List;

public class MultiKey implements CborSerializable {
    public static final int THRESHOLD_KEY = 1;
    public static final int KEYS_KEY = 2;

    private final int threshold;
    private final List<CryptoECKey> ecKeys;
    private final List<CryptoHDKey> hdKeys;

    public MultiKey(int threshold, List<CryptoECKey> ecKeys, List<CryptoHDKey> hdKeys) {
        this.threshold = threshold;
        this.ecKeys = ecKeys;
        this.hdKeys = hdKeys;
    }

    public int getThreshold() {
        return threshold;
    }

    public List<CryptoECKey> getEcKeys() {
        return ecKeys;
    }

    public List<CryptoHDKey> getHdKeys() {
        return hdKeys;
    }

    public DataItem toCbor() {
        Map map = new Map();
        map.put(new UnsignedInteger(THRESHOLD_KEY), new UnsignedInteger(threshold));
        Array array = new Array();
        if(ecKeys != null && !ecKeys.isEmpty()) {
            for(CryptoECKey cryptoECKey : ecKeys) {
                DataItem eckeyItem = cryptoECKey.toCbor();
                eckeyItem.setTag(RegistryType.CRYPTO_ECKEY.getTag());
                array.add(eckeyItem);
            }
        } else if(hdKeys != null) {
            for(CryptoHDKey cryptoHDKey : hdKeys) {
                DataItem hdkeyItem = cryptoHDKey.toCbor();
                hdkeyItem.setTag(RegistryType.CRYPTO_HDKEY.getTag());
                array.add(hdkeyItem);
            }
        }
        map.put(new UnsignedInteger(KEYS_KEY), array);
        return map;
    }

    public static MultiKey fromCbor(DataItem item) {
        int threshold = 0;
        List<CryptoECKey> ecKeys = new ArrayList<>();
        List<CryptoHDKey> hdKeys = new ArrayList<>();

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger intKey = (UnsignedInteger)key;
            if(intKey.getValue().intValue() == THRESHOLD_KEY) {
                threshold = ((UnsignedInteger)map.get(key)).getValue().intValue();
            }
            if(intKey.getValue().intValue() == KEYS_KEY) {
                Array keysArray = (Array)map.get(key);
                for(DataItem keyExp : keysArray.getDataItems()) {
                    if(keyExp.getTag().getValue() == RegistryType.CRYPTO_ECKEY.getTag()) {
                        ecKeys.add(CryptoECKey.fromCbor(keyExp));
                    } else if(keyExp.getTag().getValue() == RegistryType.CRYPTO_HDKEY.getTag()) {
                        hdKeys.add(CryptoHDKey.fromCbor(keyExp));
                    }
                }
            }
        }

        if(ecKeys.isEmpty() && hdKeys.isEmpty()) {
            throw new IllegalStateException("One or more of eckey or hdkey must be specified");
        }

        return new MultiKey(threshold, ecKeys, hdKeys);
    }
}
