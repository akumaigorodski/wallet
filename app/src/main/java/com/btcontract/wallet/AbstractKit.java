package com.btcontract.wallet;

import com.google.common.util.concurrent.AbstractIdleService;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;

import org.bitcoinj.store.SPVBlockStore;
import javax.annotation.Nullable;
import org.bitcoinj.core.*;


public abstract class AbstractKit extends AbstractIdleService {
    public static final String PASSWORD_ASK_STARTUP = "password_ask";
    public static final String BTC_OR_SATOSHI = "btc_or_satoshi";
    public static final String DESTRUCT_CODE = "destruct_code";
    public static final String SACK_OR_TXS = "sack_or_txs";
    public static final String FEE_FACTOR = "fee_factor";
    public static final String RATES_JSON = "rates_json";
    public static final String CURRENCY = "currency";

    public volatile BlockChain blockChain;
    public volatile PeerGroup peerGroup;
    public volatile SPVBlockStore store;
    public volatile Wallet wallet;

    public void startDownload() {
        FutureCallback futureListener = new FutureCallback() {
            @Override public void onSuccess(@Nullable Object res) {
                PeerEventListener lst = new AbstractPeerEventListener();
                peerGroup.startBlockChainDownload(lst);
            }

            @Override public void onFailure(@Nullable Throwable err) {
                throw new RuntimeException(err);
            }
        };

        ListenableFuture future = peerGroup.startAsync();
        Futures.addCallback(future, futureListener);
    }
}