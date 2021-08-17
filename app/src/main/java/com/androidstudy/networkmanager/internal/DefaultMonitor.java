package com.androidstudy.networkmanager.internal;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.ConnectivityManager;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import com.androidstudy.networkmanager.Monitor;

/**
 * Created by chweya on 29/08/17.
 */

public class DefaultMonitor implements Monitor {

    private final Handler mainHandler = new Handler(Looper.getMainLooper());
    private final Context context;
    private final ConnectivityListener listener;
    private final int connectionType;

    private boolean isConnected;
    private boolean isRegistered;

    private final BroadcastReceiver connectivityReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(final Context context, Intent intent) {
            boolean wasConnected = isConnected;
            isConnected = NetworkUtil.isConnected(context, connectionType);
            if (wasConnected != isConnected) {
                emitEvent();
            }
        }
    };

    public DefaultMonitor(Context context, ConnectivityListener listener, int connectionType) {
        this.context = context;
        this.listener = listener;
        this.connectionType = connectionType;
    }

    public DefaultMonitor(Context context, ConnectivityListener listener) {
        this(context, listener, -1);
    }

    private void emitEvent() {
        Log.i("Monitor", "Network change");
        mainHandler.post(new Runnable() {
            @Override
            public void run() {
                listener.onConnectivityChanged(connectionType, isConnected, isConnected && NetworkUtil.isConnectionFast(context));
            }
        });
    }

    private void register() {
        if (isRegistered) {
            return;
        }

        Log.i("Monitor", "Registering");
        isConnected = NetworkUtil.isConnected(context, connectionType);
        //emit once
        emitEvent();
        context.registerReceiver(
                connectivityReceiver,
                new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));

        isRegistered = true;
    }

    private void unregister() {
        if (!isRegistered) {
            return;
        }

        Log.i("Monitor", "Unregistering");
        context.unregisterReceiver(connectivityReceiver);
        isRegistered = false;
    }

    @Override
    public void onStart() {
        register();
    }

    @Override
    public void onStop() {
        unregister();
    }
}

