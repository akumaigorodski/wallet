package com.btcontract.wallet;

import com.google.common.util.concurrent.AbstractIdleService;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;

import org.bitcoinj.core.listeners.PeerDataEventListener;
import org.bitcoinj.store.SPVBlockStore;
import javax.annotation.Nullable;
import org.bitcoinj.core.*;
import java.util.List;


public abstract class AbstractKit extends AbstractIdleService {
    public static final String PASSWORD_ASK_STARTUP = "password_ask";
    public static final String BTC_OR_SATOSHI = "btc_or_satoshi";
    public static final String DESTRUCT_CODE = "destruct_code";
    public static final String CURRENCY = "currency";

    public volatile BlockChain blockChain;
    public volatile PeerGroup peerGroup;
    public volatile SPVBlockStore store;
    public volatile Wallet wallet;

    public class NonePeerDataListener implements PeerDataEventListener {
        public void onBlocksDownloaded(Peer peer, Block block, @Nullable FilteredBlock fb, int left) {}
        public Message onPreMessageReceived(Peer peer, Message message) { return message; }
        public List<Message> getData(Peer peer, GetDataMessage m) { return null; }
        public void onChainDownloadStarted(Peer peer, int left) {}
    }

    public void startDownload() {
        FutureCallback futureListener = new FutureCallback() {
            @Override public void onSuccess(@Nullable Object res) {
                NonePeerDataListener listener = new NonePeerDataListener();
                peerGroup.startBlockChainDownload(listener);
            }

            @Override public void onFailure(@Nullable Throwable err) {
                throw new RuntimeException(err);
            }
        };

        ListenableFuture future = peerGroup.startAsync();
        Futures.addCallback(future, futureListener);
    }
}