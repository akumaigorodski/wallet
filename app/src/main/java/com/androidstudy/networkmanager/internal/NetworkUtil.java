package com.androidstudy.networkmanager.internal;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.telephony.TelephonyManager;
import android.util.Log;

/**
 * Created by anonymous on 8/27/17.
 */

public class NetworkUtil {

    /**
     * A utility class to determine if the application has been connected to either WIFI Or Mobile
     * Network, before we make any network request to the server.
     * <p>
     * The class uses two permission - INTERNET and ACCESS NETWORK STATE, to determine the user's
     * connection stats
     */

    private NetworkUtil() {
    }

    /**
     * Get the network info
     *
     * @param context to get NetworkInfo
     * @return {@link NetworkInfo}
     */
    public static NetworkInfo getNetworkInfo(Context context) {
        try {
            ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
            return cm.getActiveNetworkInfo();
        } catch (Exception e) {
            System.out.println("CheckConnectivity Exception: " + e.getMessage());
            Log.v("connectivity", e.toString());
            return null;
        }
    }

    /**
     * Check if there is any connection
     *
     * @param context a {@link Context}
     * @return boolean
     */
    public static boolean isConnected(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnectedOrConnecting());
    }

    public static boolean isConnected(Context context, int connectionType) {
        switch (connectionType) {
            case ConnectivityManager.TYPE_WIFI:
                return isConnectedToWifi(context);
            case ConnectivityManager.TYPE_MOBILE:
                return isConnectedToMobile(context);
            default:
                return isConnected(context);
        }
    }

    /**
     * @param context
     * @return boolean
     * <p>
     * Instead use {@link NetworkUtil#isConnected(Context)}
     */
    @Deprecated
    public static boolean isOnline(Context context) {
        return isConnected(context);
    }

    /**
     * Check if there is any connection to a Wifi network
     *
     * @param context
     * @return boolean
     */
    public static boolean isConnectedToWifi(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnected() && info.getType() == ConnectivityManager.TYPE_WIFI);
    }

    /**
     * Check if there is any connection to a mobile network
     *
     * @param context
     * @return
     */
    public static boolean isConnectedToMobile(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnected() && info.getType() == ConnectivityManager.TYPE_MOBILE);
    }

    /**
     * Check if the connection is fast
     *
     * @param context
     * @return
     */
    public static boolean isConnectionFast(Context context) {
        NetworkInfo info = getNetworkInfo(context);
        return (info != null && info.isConnected())
                && isConnectionFast(info.getType(), info.getSubtype());
    }

    /**
     * Check if the connection is fast
     *
     * @param type
     * @param subType
     * @return boolean
     * <p>
     * inspired by https://gist.github.com/emil2k/5130324
     */
    public static boolean isConnectionFast(int type, int subType) {
        if (type == ConnectivityManager.TYPE_WIFI) {
            return true;
        } else if (type == ConnectivityManager.TYPE_MOBILE) {
            switch (subType) {
                case TelephonyManager.NETWORK_TYPE_1xRTT:
                    return false; // ~ 50-100 kbps
                case TelephonyManager.NETWORK_TYPE_CDMA:
                    return false; // ~ 14-64 kbps
                case TelephonyManager.NETWORK_TYPE_EDGE:
                    return false; // ~ 50-100 kbps
                case TelephonyManager.NETWORK_TYPE_EVDO_0:
                    return true; // ~ 400-1000 kbps
                case TelephonyManager.NETWORK_TYPE_EVDO_A:
                    return true; // ~ 600-1400 kbps
                case TelephonyManager.NETWORK_TYPE_GPRS:
                    return false; // ~ 100 kbps
                case TelephonyManager.NETWORK_TYPE_HSDPA:
                    return true; // ~ 2-14 Mbps
                case TelephonyManager.NETWORK_TYPE_HSPA:
                    return true; // ~ 700-1700 kbps
                case TelephonyManager.NETWORK_TYPE_HSUPA:
                    return true; // ~ 1-23 Mbps
                case TelephonyManager.NETWORK_TYPE_UMTS:
                    return true; // ~ 400-7000 kbps
                case TelephonyManager.NETWORK_TYPE_EHRPD: // API level 11
                    return true; // ~ 1-2 Mbps
                case TelephonyManager.NETWORK_TYPE_EVDO_B: // API level 9
                    return true; // ~ 5 Mbps
                case TelephonyManager.NETWORK_TYPE_HSPAP: // API level 13
                    return true; // ~ 10-20 Mbps
                case TelephonyManager.NETWORK_TYPE_IDEN: // API level 8
                    return false; // ~25 kbps
                case TelephonyManager.NETWORK_TYPE_LTE: // API level 11
                    return true; // ~ 10+ Mbps
                // Unknown
                case TelephonyManager.NETWORK_TYPE_UNKNOWN:
                default:
                    return false;
            }
        } else {
            return false;
        }
    }
}