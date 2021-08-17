package com.androidstudy.networkmanager.internal;

import com.androidstudy.networkmanager.Monitor;

/**
 * Created by chweya on 29/08/17.
 */

public class NoopMonitor implements Monitor {
    @Override
    public void onStart() {
        //no-op
    }

    @Override
    public void onStop() {
        //no-op
    }
}
