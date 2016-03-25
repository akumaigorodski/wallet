package com.btcontract.wallet;


public class Informer {
    public static final int SYNC = 1;
    public static final int DECSEND = 2;
    public static final int RECEIVED = 3;
    public static final int CODECHECK = 4;
    public static final int TXCONFIRMED = 5;
    public static final int PEERS = 0;

    public int tag;
    public String value;
    public Informer(String val, int tag)
    {
        this.value = val;
        this.tag = tag;
    }
}
