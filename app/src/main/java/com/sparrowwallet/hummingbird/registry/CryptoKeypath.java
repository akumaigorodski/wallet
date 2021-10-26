package com.sparrowwallet.hummingbird.registry;

import android.annotation.TargetApi;

import co.nstant.in.cbor.model.*;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringJoiner;

@TargetApi(24)
public class CryptoKeypath extends RegistryItem {
    public static final int COMPONENTS_KEY = 1;
    public static final int SOURCE_FINGERPRINT_KEY = 2;
    public static final int DEPTH_KEY = 3;

    private final List<PathComponent> components;
    private final byte[] sourceFingerprint;
    private final Integer depth;

    public CryptoKeypath(List<PathComponent> components, byte[] sourceFingerprint) {
        this(components, sourceFingerprint, 0);
    }

    public CryptoKeypath(List<PathComponent> components, byte[] sourceFingerprint, Integer depth) {
        this.components = components;
        this.sourceFingerprint = sourceFingerprint == null ? null : Arrays.copyOfRange(sourceFingerprint, sourceFingerprint.length - 4, sourceFingerprint.length);
        this.depth = depth;
    }

    public List<PathComponent> getComponents() {
        return components;
    }

    public String getPath() {
        if(components.isEmpty()) {
            return null;
        }

        StringJoiner joiner = new StringJoiner("/");
        for(PathComponent component : components) {
            joiner.add((component.isWildcard() ? "*" : component.getIndex()) + (component.isHardened() ? "'" : ""));
        }
        return joiner.toString();
    }

    public byte[] getSourceFingerprint() {
        return sourceFingerprint;
    }

    public Integer getDepth() {
        return depth;
    }

    public DataItem toCbor() {
        Map map = new Map();
        Array componentArray = new Array();
        for(PathComponent pathComponent : components) {
            if(pathComponent.isWildcard()) {
                componentArray.add(new Array());
            } else {
                componentArray.add(new UnsignedInteger(pathComponent.getIndex()));
            }
            componentArray.add(pathComponent.isHardened() ? SimpleValue.TRUE : SimpleValue.FALSE);
        }
        map.put(new UnsignedInteger(COMPONENTS_KEY), componentArray);
        if(sourceFingerprint != null) {
            map.put(new UnsignedInteger(SOURCE_FINGERPRINT_KEY), new UnsignedInteger(new BigInteger(1, sourceFingerprint)));
        }
        if(depth != null) {
            map.put(new UnsignedInteger(DEPTH_KEY), new UnsignedInteger(depth));
        }
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_KEYPATH;
    }

    public static CryptoKeypath fromCbor(DataItem item) {
        List<PathComponent> components = new ArrayList<>();
        byte[] sourceFingerprint = null;
        Integer depth = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();
            if(intKey == COMPONENTS_KEY) {
                Array componentArray = (Array)map.get(key);
                for(int i = 0; i < componentArray.getDataItems().size(); i+=2) {
                    boolean hardened = (componentArray.getDataItems().get(i+1) == SimpleValue.TRUE);
                    DataItem pathSeg = componentArray.getDataItems().get(i);
                    if(pathSeg instanceof UnsignedInteger) {
                        UnsignedInteger uintIndex = (UnsignedInteger)pathSeg;
                        components.add(new PathComponent(uintIndex.getValue().intValue(), hardened));
                    } else if(pathSeg instanceof Array) {
                        components.add(new PathComponent(hardened));
                    }
                }
            } else if(intKey == SOURCE_FINGERPRINT_KEY) {
                sourceFingerprint = bigIntegerToBytes(((UnsignedInteger)map.get(key)).getValue(), 4);
            } else if(intKey == DEPTH_KEY) {
                depth = ((UnsignedInteger)map.get(key)).getValue().intValue();
            }
        }

        return new CryptoKeypath(components, sourceFingerprint, depth);
    }
}
