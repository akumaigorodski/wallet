package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.UnsignedInteger;

public class CryptoCoinInfo extends RegistryItem {
    public static final int TYPE_KEY = 1;
    public static final int NETWORK_KEY = 2;

    private final Integer type;
    private final Integer network;

    public CryptoCoinInfo(Integer type, Integer network) {
        this.type = type;
        this.network = network;
    }

    public CryptoCoinInfo(Type type, Network network) {
        this.type = (type != null ? type.ordinal() : null);
        this.network = (network != null ? network.ordinal() : null);
    }

    public Type getType() {
        return type == null ? Type.BITCOIN : Type.values()[type];
    }

    public Network getNetwork() {
        return network == null ? Network.MAINNET : Network.values()[network];
    }

    public DataItem toCbor() {
        Map map = new Map();
        if(type != null) {
            map.put(new UnsignedInteger(TYPE_KEY), new UnsignedInteger(type));
        }
        if(network != null) {
            map.put(new UnsignedInteger(NETWORK_KEY), new UnsignedInteger(network));
        }
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_COIN_INFO;
    }

    public static CryptoCoinInfo fromCbor(DataItem item) {
        Integer type = null;
        Integer network = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();

            if(intKey == TYPE_KEY) {
                type = ((UnsignedInteger)map.get(key)).getValue().intValue();
            } else if(intKey == NETWORK_KEY) {
                network = ((UnsignedInteger)map.get(key)).getValue().intValue();
            }
        }

        return new CryptoCoinInfo(type, network);
    }

    public enum Type {
        BITCOIN
    }

    public enum Network {
        MAINNET, TESTNET
    }
}
