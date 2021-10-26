package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.*;

import java.math.BigInteger;
import java.util.Arrays;

public class CryptoHDKey extends RegistryItem {
    public static final int IS_MASTER_KEY = 1;
    public static final int IS_PRIVATE_KEY = 2;
    public static final int KEY_DATA_KEY = 3;
    public static final int CHAIN_CODE_KEY = 4;
    public static final int USE_INFO_KEY = 5;
    public static final int ORIGIN_KEY = 6;
    public static final int CHILDREN_KEY = 7;
    public static final int PARENT_FINGERPRINT_KEY = 8;
    public static final int NAME_KEY = 9;
    public static final int NOTE_KEY = 10;

    private final boolean master;
    private final Boolean privateKey;
    private final byte[] key;
    private final byte[] chainCode;
    private final CryptoCoinInfo useInfo;
    private final CryptoKeypath origin;
    private final CryptoKeypath children;
    private final byte[] parentFingerprint;
    private final String name;
    private final String note;

    public CryptoHDKey(byte[] key, byte[] chainCode) {
        this.master = true;
        this.privateKey = true;
        this.key = key;
        this.chainCode = chainCode;
        this.useInfo = null;
        this.origin = null;
        this.children = null;
        this.parentFingerprint = null;
        this.name = null;
        this.note = null;
    }

    public CryptoHDKey(Boolean privateKey, byte[] key, byte[] chainCode, CryptoCoinInfo useInfo, CryptoKeypath origin, CryptoKeypath children, byte[] parentFingerprint) {
        this(privateKey, key, chainCode, useInfo, origin, children, parentFingerprint, null, null);
    }

    public CryptoHDKey(Boolean privateKey, byte[] key, byte[] chainCode, CryptoCoinInfo useInfo, CryptoKeypath origin, CryptoKeypath children, byte[] parentFingerprint, String name, String note) {
        this.master = false;
        this.privateKey = privateKey;
        this.key = key;
        this.chainCode = chainCode;
        this.useInfo = useInfo;
        this.origin = origin;
        this.children = children;
        this.parentFingerprint = parentFingerprint == null ? null : Arrays.copyOfRange(parentFingerprint, parentFingerprint.length - 4, parentFingerprint.length);
        this.name = name;
        this.note = note;
    }

    public boolean isMaster() {
        return master;
    }

    public boolean isPrivateKey() {
        return privateKey == null ? false : privateKey;
    }

    public byte[] getKey() {
        return key;
    }

    public byte[] getChainCode() {
        return chainCode;
    }

    public CryptoCoinInfo getUseInfo() {
        return useInfo;
    }

    public CryptoKeypath getOrigin() {
        return origin;
    }

    public CryptoKeypath getChildren() {
        return children;
    }

    public byte[] getParentFingerprint() {
        return parentFingerprint;
    }

    public String getName() {
        return name;
    }

    public String getNote() {
        return note;
    }

    public DataItem toCbor() {
        Map map = new Map();
        if(master) {
            map.put(new UnsignedInteger(IS_MASTER_KEY), SimpleValue.TRUE);
            map.put(new UnsignedInteger(KEY_DATA_KEY), new ByteString(key));
            map.put(new UnsignedInteger(CHAIN_CODE_KEY), new ByteString(chainCode));
        } else {
            if(privateKey != null) {
                map.put(new UnsignedInteger(IS_PRIVATE_KEY), privateKey ? SimpleValue.TRUE : SimpleValue.FALSE);
            }
            map.put(new UnsignedInteger(KEY_DATA_KEY), new ByteString(key));
            if(chainCode != null) {
                map.put(new UnsignedInteger(CHAIN_CODE_KEY), new ByteString(chainCode));
            }
            if(useInfo != null) {
                DataItem useInfoItem = useInfo.toCbor();
                useInfoItem.setTag(RegistryType.CRYPTO_COIN_INFO.getTag());
                map.put(new UnsignedInteger(USE_INFO_KEY), useInfoItem);
            }
            if(origin != null) {
                DataItem originItem = origin.toCbor();
                originItem.setTag(RegistryType.CRYPTO_KEYPATH.getTag());
                map.put(new UnsignedInteger(ORIGIN_KEY), originItem);
            }
            if(children != null) {
                DataItem childrenItem = children.toCbor();
                childrenItem.setTag(RegistryType.CRYPTO_KEYPATH.getTag());
                map.put(new UnsignedInteger(CHILDREN_KEY), childrenItem);
            }
            if(parentFingerprint != null) {
                map.put(new UnsignedInteger(PARENT_FINGERPRINT_KEY), new UnsignedInteger(new BigInteger(1, parentFingerprint)));
            }
            if(name != null) {
                map.put(new UnsignedInteger(NAME_KEY), new UnicodeString(name));
            }
            if(note != null) {
                map.put(new UnsignedInteger(NOTE_KEY), new UnicodeString(note));
            }
        }
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_HDKEY;
    }

    public static CryptoHDKey fromCbor(DataItem item) {
        boolean isMasterKey = false;
        Boolean isPrivateKey = null;
        byte[] keyData = null;
        byte[] chainCode = null;
        CryptoCoinInfo useInfo = null;
        CryptoKeypath origin = null;
        CryptoKeypath children = null;
        byte[] parentFingerprint = null;
        String name = null;
        String note = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();
            if(intKey == IS_MASTER_KEY) {
                isMasterKey = (map.get(uintKey) == SimpleValue.TRUE);
            } else if(intKey == IS_PRIVATE_KEY) {
                isPrivateKey = (map.get(uintKey) == SimpleValue.TRUE);
            } else if(intKey == KEY_DATA_KEY) {
                keyData = ((ByteString)map.get(uintKey)).getBytes();
            } else if(intKey == CHAIN_CODE_KEY) {
                chainCode = ((ByteString)map.get(uintKey)).getBytes();
            } else if(intKey == USE_INFO_KEY) {
                useInfo = CryptoCoinInfo.fromCbor(map.get(uintKey));
            } else if(intKey == ORIGIN_KEY) {
                origin = CryptoKeypath.fromCbor(map.get(uintKey));
            } else if(intKey == CHILDREN_KEY) {
                children = CryptoKeypath.fromCbor(map.get(uintKey));
            } else if(intKey == PARENT_FINGERPRINT_KEY) {
                parentFingerprint = bigIntegerToBytes(((UnsignedInteger)map.get(uintKey)).getValue(), 4);
            } else if(intKey == NAME_KEY) {
                name = ((UnicodeString)map.get(uintKey)).getString();
            } else if(intKey == NOTE_KEY) {
                note = ((UnicodeString)map.get(uintKey)).getString();
            }
        }

        if(keyData == null) {
            throw new IllegalStateException("Key data is null");
        }

        if(isMasterKey) {
            if(chainCode == null) {
                throw new IllegalStateException("Chain code data is null");
            }

            return new CryptoHDKey(keyData, chainCode);
        } else {
            return new CryptoHDKey(isPrivateKey, keyData, chainCode, useInfo, origin, children, parentFingerprint, name, note);
        }
    }
}
