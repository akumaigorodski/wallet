package com.androidstudy.networkmanager;

import android.content.Context;

import androidx.annotation.NonNull;

/**
 * Created by chweya on 29/08/17.
 */

public interface MonitorFactory {

    @NonNull
    Monitor create(
            @NonNull Context context,
            int connectionType,
            @NonNull Monitor.ConnectivityListener listener);
}
