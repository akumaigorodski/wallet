package com.sparrowwallet.hummingbird.registry;

import co.nstant.in.cbor.model.*;

import java.util.Date;

public class CryptoSeed extends RegistryItem {
    public static final long PAYLOAD_KEY = 1;
    public static final long BIRTHDATE_KEY = 2;
    public static final long NAME_KEY = 3;
    public static final long NOTE_KEY = 4;

    private final byte[] seed;
    private final Date birthdate;
    private final String name;
    private final String note;

    public CryptoSeed(byte[] seed, Date birthdate) {
        this(seed, birthdate, null, null);
    }

    public CryptoSeed(byte[] seed, Date birthdate, String name, String note) {
        this.seed = seed;
        this.birthdate = birthdate;
        this.name = name;
        this.note = note;
    }

    public byte[] getSeed() {
        return seed;
    }

    public Date getBirthdate() {
        return birthdate;
    }

    public String getName() {
        return name;
    }

    public String getNote() {
        return note;
    }

    public DataItem toCbor() {
        Map map = new Map();
        map.put(new UnsignedInteger(PAYLOAD_KEY), new ByteString(seed));
        if(birthdate != null) {
            DataItem birthdateItem = new UnsignedInteger(birthdate.getTime() / (1000 * 60 * 60 * 24));
            birthdateItem.setTag(100);
            map.put(new UnsignedInteger(BIRTHDATE_KEY), birthdateItem);
        }
        if(name != null) {
            map.put(new UnsignedInteger(NAME_KEY), new UnicodeString(name));
        }
        if(note != null) {
            map.put(new UnsignedInteger(NOTE_KEY), new UnicodeString(note));
        }
        return map;
    }

    @Override
    public RegistryType getRegistryType() {
        return RegistryType.CRYPTO_SEED;
    }

    public static CryptoSeed fromCbor(DataItem item) {
        byte[] seed = null;
        Date birthdate = null;
        String name = null;
        String note = null;

        Map map = (Map)item;
        for(DataItem key : map.getKeys()) {
            UnsignedInteger uintKey = (UnsignedInteger)key;
            int intKey = uintKey.getValue().intValue();
            if(intKey == PAYLOAD_KEY) {
                seed = ((ByteString)map.get(key)).getBytes();
            } else if(intKey == BIRTHDATE_KEY) {
                birthdate = new Date(((UnsignedInteger)map.get(key)).getValue().longValue() * 1000 * 60 * 60 * 24);
            } else if(intKey == NAME_KEY) {
                name = ((UnicodeString)map.get(key)).getString();
            } else if(intKey == NOTE_KEY) {
                note = ((UnicodeString)map.get(key)).getString();
            }
        }

        if(seed == null) {
            throw new IllegalStateException("Seed is null");
        }

        return new CryptoSeed(seed, birthdate, name, note);
    }
}
