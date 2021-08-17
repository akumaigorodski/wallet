package com.androidstudy.networkmanager;

import android.content.Context;
import android.util.Log;

import com.androidstudy.networkmanager.internal.DefaultMonitorFactory;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by chweya on 29/08/17.
 */

public class Tovuti {
    private static final String TAG = "Tovuti";
    private static final Object lock = new Object();

    private static volatile Tovuti tovuti;
    private WeakReference<Context> contextRef;
    private Set<Monitor> monitors;

    private Tovuti(Context context) {
        monitors = new HashSet<>();
        this.contextRef = new WeakReference<>(context);
    }

    public static Tovuti from(Context context) {
        if (tovuti == null) {
            synchronized (lock) {
                if (tovuti == null) {
                    tovuti = new Tovuti(context);
                }
            }
        }
        return tovuti;
    }

    public Tovuti monitor(int connectionType, Monitor.ConnectivityListener listener) {
        Context context = contextRef.get();
        if (context != null)
            monitors.add(new DefaultMonitorFactory().create(context, connectionType, listener));

        start();
        return tovuti;
    }

    public Tovuti monitor(Monitor.ConnectivityListener listener) {
        return monitor(-1, listener);
    }

    public void start() {
        for (Monitor monitor : monitors) {
            monitor.onStart();
        }

        if (monitors.size() > 0)
            Log.i(TAG, "started tovuti");
    }

    public void stop() {
        for (Monitor monitor : monitors) {
            monitor.onStop();
        }
    }
}
