package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.*;

public class CryptoAddress extends RegistryItem {
    public static final long INFO = 1;
    public static final long TYPE = 2;
    public static final long DATA = 3;

    private final CryptoCoinInfo info;
    private final Type type;
    private final byte[] data;

    public CryptoAddress(CryptoCoinInfo info, Type type, byte[] data) {
        this.info = info;
        this.type = type;
        this.data = data;
    }

    public CryptoCoinInfo getInfo() {
        return info;
    }

    public Type getType() {
        return type;
    }

    public byte[] getData() {
        return data;
    }

    public DataItem toCbor() {
        Map map = new Map();
        if(info != null) {
            map.put(new UnsignedInteger(INFO), info.toCbor());
        }
        if(type != null) {
            map.put(new UnsignedInteger(TYPE), new UnsignedInteger(type.ordinal()));
        }
        map.put(new UnsignedInteger(DATA), new ByteString(data));
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_ADDRESS;
    }

    public static CryptoAddress fromCbor(DataItem item) {
        CryptoCoinInfo info = null;
        Type type = null;
        byte[] data = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();
            if(intKey == INFO) {
                info = CryptoCoinInfo.fromCbor(map.get(key));
            } else if(intKey == TYPE) {
                type = Type.values()[((UnsignedInteger)map.get(key)).getValue().intValue()];
            } else if(intKey == DATA) {
                data = ((ByteString)map.get(key)).getBytes();
            }
        }

        if(data == null) {
            throw new IllegalStateException("Data is null");
        }

        return new CryptoAddress(info, type, data);
    }

    public enum Type {
        P2PKH, P2SH, P2WPKH
    }
}
