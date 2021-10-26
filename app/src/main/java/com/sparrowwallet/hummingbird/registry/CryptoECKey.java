package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.*;

public class CryptoECKey extends RegistryItem {
    public static final long CURVE = 1;
    public static final long PRIVATE = 2;
    public static final long DATA = 3;

    private final Integer curve;
    private final Boolean privateKey;
    private final byte[] data;

    public CryptoECKey(Integer curve, Boolean privateKey, byte[] data) {
        this.curve = curve;
        this.privateKey = privateKey;
        this.data = data;
    }

    public int getCurve() {
        return curve == null ? 0 : curve;
    }

    public boolean isPrivateKey() {
        return privateKey == null ? false : privateKey;
    }

    public byte[] getData() {
        return data;
    }

    public DataItem toCbor() {
        Map map = new Map();
        if(curve != null) {
            map.put(new UnsignedInteger(CURVE), new UnsignedInteger(curve));
        }
        if(privateKey != null) {
            map.put(new UnsignedInteger(PRIVATE), privateKey ? SimpleValue.TRUE : SimpleValue.FALSE);
        }
        map.put(new UnsignedInteger(DATA), new ByteString(data));
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_ECKEY;
    }

    public static CryptoECKey fromCbor(DataItem item) {
        Integer curve = null;
        Boolean privateKey = null;
        byte[] data = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();
            if(intKey == CURVE) {
                curve = ((UnsignedInteger)map.get(key)).getValue().intValue();
            } else if(intKey == PRIVATE) {
                privateKey = (map.get(key) == SimpleValue.TRUE);
            } else if(intKey == DATA) {
                data = ((ByteString)map.get(key)).getBytes();
            }
        }

        if(data == null) {
            throw new IllegalStateException("Data is null");
        }

        return new CryptoECKey(curve, privateKey, data);
    }
}
