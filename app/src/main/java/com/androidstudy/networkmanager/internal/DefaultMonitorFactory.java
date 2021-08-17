package com.androidstudy.networkmanager.internal;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;

import com.androidstudy.networkmanager.Monitor;
import com.androidstudy.networkmanager.MonitorFactory;

/**
 * Created by chweya on 29/08/17.
 */

public class DefaultMonitorFactory implements MonitorFactory {
    public static final String ACCESS_NETWORK_PERMISSION = Manifest.permission.ACCESS_NETWORK_STATE;

    @NonNull
    @Override
    public Monitor create(
            @NonNull Context context,
            int connectionType,
            @NonNull Monitor.ConnectivityListener listener) {

        int permissionResult = ContextCompat.checkSelfPermission(context, ACCESS_NETWORK_PERMISSION);
        boolean hasPermission = permissionResult == PackageManager.PERMISSION_GRANTED;

        return hasPermission ? new DefaultMonitor(context, listener, connectionType)
                : new NoopMonitor();
    }
}
