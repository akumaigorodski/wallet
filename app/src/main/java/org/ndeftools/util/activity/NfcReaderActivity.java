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

import org.ndeftools.Message;
import org.ndeftools.Record;
import org.ndeftools.UnsupportedRecord;
import org.ndeftools.wellknown.TextRecord;

import android.app.Activity;
import android.content.Intent;
import android.nfc.FormatException;
import android.nfc.NdefMessage;
import android.nfc.NdefRecord;
import android.nfc.NfcAdapter;
import android.os.Parcelable;
import android.util.Log;


/**
 * 
 * Abstract {@link Activity} for reading NFC messages - both via a tag and via Beam (push)
 * 
 * @author Thomas Rorvik Skjolberg
 *
 */

public abstract class NfcReaderActivity extends NfcDetectorActivity {

	private static final String TAG = NfcReaderActivity.class.getName();

	@Override
	public void nfcIntentDetected(Intent intent, String action) {
		Log.d(TAG, "nfcIntentDetected: " + action);
		
		Parcelable[] messages = intent.getParcelableArrayExtra(NfcAdapter.EXTRA_NDEF_MESSAGES);
		if (messages != null) {
			NdefMessage[] ndefMessages = new NdefMessage[messages.length];
		    for (int i = 0; i < messages.length; i++) {
		        ndefMessages[i] = (NdefMessage) messages[i];
		    }
		    
		    if(ndefMessages.length > 0) {
		    	// read as much as possible
				Message message = new Message();
				for (int i = 0; i < messages.length; i++) {
			    	NdefMessage ndefMessage = (NdefMessage) messages[i];
			        
					for(NdefRecord ndefRecord : ndefMessage.getRecords()) {
						try {
							message.add(Record.parse(ndefRecord));
						} catch (FormatException e) {
							// if the record is unsupported or corrupted, keep as unsupported record
							message.add(UnsupportedRecord.parse(ndefRecord));
						}
					}
			    }
				readNdefMessage(message);
		    } else {
		    	readEmptyNdefMessage();
		    }
		} else  {
			readNonNdefMessage();
		}
	}
	
	/**
	 * An NDEF message was read and parsed
	 * 
	 * @param message the message
	 */
	
	protected abstract void readNdefMessage(Message message);

	/**
	 * An empty NDEF message was read.
	 * 
	 */
	
	protected abstract void readEmptyNdefMessage();

	/**
	 * 
	 * Something was read via NFC, but it was not an NDEF message. 
	 * 
	 * Handling this situation is out of scope of this project.
	 * 
	 */
	
	protected abstract void readNonNdefMessage();


	public String ndefMessageString(Message message) {
		Log.d(TAG, "Found " + message.size() + " NDEF records");

		for(int k = 0; k < message.size(); k++) {
			Record record = message.get(k);

			if (record instanceof TextRecord) {
				return ((TextRecord) record).getText();
			}
		}

		return null;
	}
}
