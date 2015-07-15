package com.btcontract.wallet;


public class Informer {
    public static final int SYNC = 0;
    public static final int REORG = 1;
    public static final int PEERS = 2;
    public static final int DECSEND = 3;
    public static final int RECEIVED = 4;
    public static final int CODECHANGE = 5;
    public static final int TXCONFIRMED = 6;

    public int tag;
    public String value;
    public Informer(String val, int tag)
    {
        this.value = val;
        this.tag = tag;
    }
}
