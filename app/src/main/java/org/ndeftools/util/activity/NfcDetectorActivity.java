/***************************************************************************
 * 
 * This file is part of the 'NDEF Tools for Android' project at
 * http://code.google.com/p/ndef-tools-for-android/
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 ****************************************************************************/

package org.ndeftools.util.activity;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.nfc.NfcAdapter;
import android.os.Bundle;
import android.util.Log;

import androidx.appcompat.app.AppCompatActivity;


/**
 * 
 * Abstract {@link Activity} for detecting incoming NFC messages.<br/><br/>
 * 
 *  - detects whether NFC is available (if device has NFC chip).<br/>
 *  - detect whether NFC setting is on or off, and whether it changes from off to on or on to off.<br/>
 *  - detect incoming data tags or beams.<br/>
 * 
 * @author Thomas Rorvik Skjolberg
 *
 */

public abstract class NfcDetectorActivity extends AppCompatActivity {
	
	/**
     * Broadcast Action: The state of the local NFC adapter has been
     * changed.
     * <p>For example, NFC has been turned on or off.
     * <p>Always contains the extra field
     */
	
	public static final String ACTION_ADAPTER_STATE_CHANGED = "android.nfc.action.ADAPTER_STATE_CHANGED";
	
    public static final String EXTRA_ADAPTER_STATE = "android.nfc.extra.ADAPTER_STATE";
    public static final String STATE_INTENT_PROCESSED = "org.ndeftools.util.activity.state.intent_processed";
    
    public static final int STATE_OFF = 1;
    public static final int STATE_ON = 3;
    
    private static final String ACTION_NFC_SETTINGS = "android.settings.NFC_SETTINGS";
    
    /** this action seems never to be emitted, but is here for future use */
    private static final String ACTION_TAG_LEFT_FIELD = "android.nfc.action.TAG_LOST";
    
    private static final String TAG = NfcDetectorActivity.class.getName();
    
    private static IntentFilter nfcStateChangeIntentFilter = new IntentFilter(ACTION_ADAPTER_STATE_CHANGED);
    
	protected NfcAdapter nfcAdapter;
	protected IntentFilter[] writeTagFilters;
	protected PendingIntent nfcPendingIntent;
	
	protected boolean foreground = false;
	protected boolean intentProcessed = false;
	protected boolean nfcEnabled = false;
	
	protected BroadcastReceiver nfcStateChangeBroadcastReceiver;

	protected boolean detecting = false;
	
	/** NXP chips support Mifare Classic, others do not. */
	protected boolean nxpMifareClassic;

    public void initNfc(Bundle savedInstanceState) {
    	
    	if(savedInstanceState != null) {
    	    intentProcessed = savedInstanceState.getBoolean(STATE_INTENT_PROCESSED);
    	}
    	
    	nxpMifareClassic = hasMifareClassic();
    	 
    	// Check for available NFC Adapter
    	PackageManager pm = getPackageManager();
    	if(pm.hasSystemFeature(PackageManager.FEATURE_NFC) && NfcAdapter.getDefaultAdapter(this) != null) {
        	Log.d(TAG, "NFC feature found");

    		onNfcFeatureFound();
    	} else {
        	Log.d(TAG, "NFC feature not found");

    		onNfcFeatureNotFound();
    	}
    	
    }

	private boolean hasMifareClassic() {
		return getPackageManager().hasSystemFeature("com.nxp.mifare");
	}

    /**
     * Notify that NFC is available
     */
    
    protected void onNfcFeatureFound() {    	
		initializeNfc();
		detectInitialNfcState();
    }

    /**
     * 
     * Initialize Nfc fields
     * 
     */
    
	protected void initializeNfc() {
		nfcAdapter = NfcAdapter.getDefaultAdapter(this);
		nfcPendingIntent = PendingIntent.getActivity(this, 0, new Intent(this, this.getClass()).addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP), 0);
		
        IntentFilter tagDetected = new IntentFilter(NfcAdapter.ACTION_TAG_DISCOVERED);
        IntentFilter ndefDetected = new IntentFilter(NfcAdapter.ACTION_NDEF_DISCOVERED);
        IntentFilter techDetected = new IntentFilter(NfcAdapter.ACTION_TECH_DISCOVERED);
        IntentFilter tagLost = new IntentFilter(ACTION_TAG_LEFT_FIELD);
        
        writeTagFilters = new IntentFilter[] {ndefDetected, tagDetected, techDetected, tagLost};
        
        nfcStateChangeBroadcastReceiver = new BroadcastReceiver() {
			@Override
			public void onReceive(Context context, Intent intent) {
				final int state = intent.getIntExtra(EXTRA_ADAPTER_STATE, -1);
				if(state == STATE_OFF || state == STATE_ON) {
					
					runOnUiThread(new Runnable() {
					    public void run() {
							if(state == STATE_ON) {
								if(detecting) {
									enableForeground();
								}
							} 

					    	detectNfcStateChanges();
					    }
					});
				}
			}
		};
	}
	
	/**
	 * 
	 * Detect initial NFC state.
	 * 
	 */
    
    protected void detectInitialNfcState() {
    	nfcEnabled = nfcAdapter.isEnabled();
		if(nfcEnabled) {
	    	Log.d(TAG, "NFC is enabled");

			onNfcStateEnabled();
		} else {
	    	Log.d(TAG, "NFC is disabled"); // change state in wireless settings
	    	
			onNfcStateDisabled();
		}
	}
    
    /**
     * 
     * NFC feature was found and is currently enabled
     * 
     */
    
    protected abstract void onNfcStateEnabled();

    /**
     * 
     * NFC feature was found but is currently disabled
     * 
     */

    protected abstract void onNfcStateDisabled();

    @Override
    protected void onResume() {
    	super.onResume();
    	
    	if(nfcAdapter != null) {
    		// enable foreground mode if nfc is on and we have started detecting
        	boolean enabled = nfcAdapter.isEnabled();
    		if(enabled && detecting) {
    			enableForeground();
    		}
    		
    		detectNfcStateChanges();
    		
    		// for quicksettings
    		startDetectingNfcStateChanges();
    	}
    	
		if(!intentProcessed) {
			intentProcessed = true;
			
			processIntent();
		}
    	
    }

    /**
     * 
     * NFC setting changed since last check. For example, the user enabled NFC in the wireless settings.
     * 
     */

    protected abstract void onNfcStateChange(boolean enabled);

    /**
     * 
     * Detect changes in NFC settings - enabled/disabled
     * 
     */
    
    protected void detectNfcStateChanges() {
    	Log.d(TAG, "Detect NFC state changes while previously " + (nfcEnabled ? "enabled" : "disabled"));
    	
		boolean enabled = nfcAdapter.isEnabled();
		if(nfcEnabled != enabled) {
			Log.d(TAG, "NFC state change detected; NFC is now " + (enabled ? "enabled" : "disabled"));
			onNfcStateChange(enabled);
			
			nfcEnabled = enabled;
		} else {
			Log.d(TAG, "NFC state remains " + (enabled ? "enabled" : "disabled"));
		}
	}
    
    public void startDetectingNfcStateChanges() {
		registerReceiver(nfcStateChangeBroadcastReceiver, nfcStateChangeIntentFilter);
	}

    public void stopDetectingNfcStateChanges() {
		unregisterReceiver(nfcStateChangeBroadcastReceiver);
	}
	
	@Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putBoolean(STATE_INTENT_PROCESSED, intentProcessed);
    }
	  
    @Override
    protected void onPause() {
    	super.onPause();
    	
    	if(nfcAdapter != null) {
    		disableForeground();
    		
    		// for quicksettings
    		stopDetectingNfcStateChanges();
    	}
    }
    
    @Override
    public void onNewIntent(Intent intent) {
    	super.onNewIntent(intent);

    	intentProcessed = false;

        setIntent(intent);
    }
	
    protected void enableForeground() {
        if(!foreground) {
        	Log.d(TAG, "Enable nfc forground mode");
        	
	        nfcAdapter.enableForegroundDispatch(this, nfcPendingIntent, writeTagFilters, null);
	        
	    	foreground = true;
        }
    }

    /**
     *
     * Start detecting NDEF messages
     *
     */

    protected void startDetecting() {
    	if(!detecting) {
    		enableForeground();

    		detecting = true;
    	}
    }

    /**
     *
     * Stop detecting NDEF messages
     *
     */

    protected void stopDetecting() {
    	if(detecting) {
    		disableForeground();

    		detecting = false;
    	}
    }
    
    protected void disableForeground() {
    	if(foreground) {
        	Log.d(TAG, "Disable nfc forground mode");
        	
        	nfcAdapter.disableForegroundDispatch(this);
    	
    		foreground = false;
    	}
    }
    
    /**
     * 
     * Process the current intent, looking for NFC-related actions
     * 
     */

	public void processIntent() {
		Intent intent = getIntent();
        // Check to see that the Activity started due to an Android Beam
        if (NfcAdapter.ACTION_NDEF_DISCOVERED.equals(intent.getAction())) {

        	nfcIntentDetected(intent, NfcAdapter.ACTION_NDEF_DISCOVERED);
        } else if (NfcAdapter.ACTION_TAG_DISCOVERED.equals(intent.getAction())) {

        	nfcIntentDetected(intent, NfcAdapter.ACTION_TAG_DISCOVERED);
        } else if (NfcAdapter.ACTION_TECH_DISCOVERED.equals(intent.getAction())) {

        	nfcIntentDetected(intent, NfcAdapter.ACTION_TECH_DISCOVERED);
        } else {
			onNoNfcIntentFound();
        }
	}

	protected void onNoNfcIntentFound() {
		Log.d(TAG, "No intent found ");
	}
	
	/**
	 * 
	 * Launch an activity for NFC (or wireless) settings, so that the user might enable or disable nfc
	 * 
	 */

	
	protected void startNfcSettingsActivity() {
		startActivity(new Intent(ACTION_NFC_SETTINGS));
	}
	
	/**
	 * 
	 * Incoming NFC communication (in form of tag or beam) detected
	 * 
	 */

	
	protected abstract void nfcIntentDetected(Intent intent, String action);

	/**
	 * 
	 * This device does not have NFC hardware
	 * 
	 */
	
    protected abstract void onNfcFeatureNotFound();

	public boolean isDetecting() {
		return detecting;
	}

	public void setDetecting(boolean detecting) {
		this.detecting = detecting;
	}
}
