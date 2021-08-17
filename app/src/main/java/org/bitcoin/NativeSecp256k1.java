/*
 * Copyright 2013 Google Inc.
 * Copyright 2014-2016 the libsecp256k1 contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bitcoin;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.math.BigInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import static org.bitcoin.NativeSecp256k1Util.*;

/**
 * <p>This class holds native methods to handle ECDSA verification.</p>
 *
 * <p>You can find an example library that can be used for this at https://github.com/bitcoin/secp256k1</p>
 *
 * <p>To build secp256k1 for use with bitcoinj, run
 * `./configure --enable-jni --enable-experimental --enable-module-ecdh`
 * and `make` then copy `.libs/libsecp256k1.so` to your system library path
 * or point the JVM to the folder containing it with -Djava.library.path
 * </p>
 */
public class NativeSecp256k1 {
    private static final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private static final Lock r = rwl.readLock();
    private static final Lock w = rwl.writeLock();
    private static ThreadLocal<ByteBuffer> nativeECDSABuffer = new ThreadLocal<ByteBuffer>();

    private static ByteBuffer pack(byte[]... buffers) {
        int size = 0;
        for(int i = 0; i < buffers.length; i++) {
            size += buffers[i].length;
        }
        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < size) {
            byteBuff = ByteBuffer.allocateDirect(size);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        for(int i = 0; i < buffers.length; i++) {
            byteBuff.put(buffers[i]);
        }
        return byteBuff;
    }

    /**
     * Verifies the given secp256k1 signature in native code.
     * Calling when enabled == false is undefined (probably library not loaded)
     *
     * @param data      The data which was signed, must be exactly 32 bytes
     * @param signature The signature
     * @param pub       The public key which did the signing
     * @return true if the signature is valid
     * @throws AssertFailException in case of failure
     */
    public static boolean verify(byte[] data, byte[] signature, byte[] pub) throws AssertFailException {
        checkArgument(data.length == 32 && signature.length <= 520 && pub.length <= 520);
        ByteBuffer byteBuff = pack(data, signature, pub);

        r.lock();
        try {
            return secp256k1_ecdsa_verify(byteBuff, Secp256k1Context.getContext(), signature.length, pub.length) == 1;
        } finally {
            r.unlock();
        }
    }

    /**
     * libsecp256k1 Create an ECDSA signature.
     *
     * @param data Message hash, 32 bytes
     * @param sec  Secret key, 32 bytes
     * @return a signature, or an empty array is signing failed
     * @throws AssertFailException in case of failure
     */
    public static byte[] sign(byte[] data, byte[] sec) throws AssertFailException {
        checkArgument(data.length == 32 && sec.length <= 32);
        ByteBuffer byteBuff = pack(data, sec);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_sign(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int sigLen = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(sigArr.length, sigLen, "Got bad signature length.");

        return retVal == 0 ? new byte[0] : sigArr;
    }

    /**
     * libsecp256k1 Create an ECDSA signature.
     *
     * @param data Message hash, 32 bytes
     * @param sec  Secret key, 32 bytes
     *             <p>
     *             Return values
     * @return a signature, or an empty array is signing failed
     * @throws AssertFailException in case of failure
     */
    public static byte[] signCompact(byte[] data, byte[] sec) throws AssertFailException {
        checkArgument(data.length == 32 && sec.length <= 32);
        ByteBuffer byteBuff = pack(data, sec);
        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_sign_compact(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int sigLen = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(sigArr.length, sigLen, "Got bad signature length.");

        return retVal == 0 ? new byte[0] : sigArr;
    }

    /**
     * libsecp256k1 Seckey Verify - returns 1 if valid, 0 if invalid
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @return true if seckey is valid
     */
    public static boolean secKeyVerify(byte[] seckey) {
        checkArgument(seckey.length == 32);
        ByteBuffer byteBuff = pack(seckey);

        r.lock();
        try {
            return secp256k1_ec_seckey_verify(byteBuff, Secp256k1Context.getContext()) == 1;
        } finally {
            r.unlock();
        }
    }


    /**
     * libsecp256k1 Compute Pubkey - computes public key from secret key
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @throws AssertFailException if parameters are not valid
     * @return the corresponding public key (compressed or uncompressed)
     */
    public static byte[] computePubkey(byte[] seckey) throws AssertFailException {
        checkArgument(seckey.length == 32);
        ByteBuffer byteBuff = pack(seckey);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ec_pubkey_create(byteBuff, Secp256k1Context.getContext(), 1);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];
        int pubLen = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        byte[] pub = retVal == 0 ? new byte[0] : pubArr;

        return pub;
    }

    /**
     * @param pubkey public key
     * @return the input public key (uncompressed) if valid, or an empty array
     * @throws AssertFailException in case of failure
     */
    public static byte[] parsePubkey(byte[] pubkey) throws AssertFailException {
        checkArgument(pubkey.length == 33 || pubkey.length == 65);
        ByteBuffer byteBuff = pack(pubkey);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ec_pubkey_parse(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];
        int pubLen = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(pubArr.length, 65, "Got bad pubkey length.");

        return retVal == 0 ? new byte[0] : pubArr;
    }

    /**
     * libsecp256k1 Cleanup - This destroys the secp256k1 context object
     * This should be called at the end of the program for proper cleanup of the context.
     */
    public static synchronized void cleanup() {
        w.lock();
        try {
            secp256k1_destroy_context(Secp256k1Context.getContext());
        } finally {
            w.unlock();
        }
    }

    public static long cloneContext() {
        r.lock();
        try {
            return secp256k1_ctx_clone(Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }
    }

    public static byte[] privKeyNegate(byte[] privkey) throws AssertFailException {
        checkArgument(privkey.length == 32);
        ByteBuffer byteBuff = pack(privkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_privkey_negate(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] privArr = retByteArray[0];

        int privLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(privArr.length, privLen, "Got bad privkey length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return privArr;
    }
    /**
     * libsecp256k1 PrivKey Tweak-Mul - Tweak privkey by multiplying to it
     *
     * @param privkey 32-byte seckey
     * @param tweak   some bytes to tweak with
     * @return privkey * tweak
     * @throws AssertFailException in case of failure
     */
    public static byte[] privKeyTweakMul(byte[] privkey, byte[] tweak) throws AssertFailException {
        checkArgument(privkey.length == 32);
        ByteBuffer byteBuff = pack(privkey, tweak);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_privkey_tweak_mul(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] privArr = retByteArray[0];

        int privLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(privArr.length, privLen, "Got bad privkey length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return privArr;
    }

    /**
     * libsecp256k1 PrivKey Tweak-Add - Tweak privkey by adding to it
     *
     * @param privkey 32-byte seckey
     * @param tweak  some bytes to tweak with
     * @return privkey + tweak
     * @throws AssertFailException in case of failure
     */
    public static byte[] privKeyTweakAdd(byte[] privkey, byte[] tweak) throws AssertFailException {
        checkArgument(privkey.length == 32);
        ByteBuffer byteBuff = pack(privkey, tweak);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_privkey_tweak_add(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] privArr = retByteArray[0];

        int privLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(privArr.length, privLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return privArr;
    }

    public static byte[] pubKeyNegate(byte[] pubkey) throws AssertFailException {
        checkArgument(pubkey.length == 33 || pubkey.length == 65);
        ByteBuffer byteBuff = pack(pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_pubkey_negate(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 PubKey Tweak-Add - Tweak pubkey by adding to it
     *
     * @param tweak  some bytes to tweak with
     * @param pubkey 32-byte seckey
     * @return pubkey + tweak
     * @throws AssertFailException in case of failure
     */
    public static byte[] pubKeyTweakAdd(byte[] pubkey, byte[] tweak) throws AssertFailException {
        checkArgument(pubkey.length == 33 || pubkey.length == 65);
        ByteBuffer byteBuff = pack(pubkey, tweak);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_pubkey_tweak_add(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 PubKey Tweak-Mul - Tweak pubkey by multiplying to it
     *
     * @param tweak  some bytes to tweak with
     * @param pubkey 32-byte seckey
     * @return pubkey * tweak
     * @throws AssertFailException in case of failure
     */
    public static byte[] pubKeyTweakMul(byte[] pubkey, byte[] tweak) throws AssertFailException {
        checkArgument(pubkey.length == 33 || pubkey.length == 65);
        ByteBuffer byteBuff = pack(pubkey, tweak);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_pubkey_tweak_mul(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    public static byte[] pubKeyAdd(byte[] pubkey1, byte[] pubkey2) throws AssertFailException {
        checkArgument(pubkey1.length == 33 || pubkey1.length == 65);
        checkArgument(pubkey2.length == 33 || pubkey2.length == 65);
        ByteBuffer byteBuff = pack(pubkey1, pubkey2);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_ec_pubkey_add(byteBuff, Secp256k1Context.getContext(), pubkey1.length, pubkey2.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[]{retByteArray[1][0]}).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[]{retByteArray[1][1]}).intValue();

        assertEquals(65, pubLen, "Got bad pubkey length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 create ECDH secret - constant time ECDH calculation
     *
     * @param seckey byte array of secret key used in exponentiaion
     * @param pubkey byte array of public key used in exponentiaion
     * @return ecdh(sedckey, pubkey)
     * @throws AssertFailException in case of failure
     */
    public static byte[] createECDHSecret(byte[] seckey, byte[] pubkey) throws AssertFailException {
        checkArgument(seckey.length <= 32 && pubkey.length <= 65);
        ByteBuffer byteBuff = pack(seckey, pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_ecdh(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] resArr = retByteArray[0];
        int retVal = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();

        assertEquals(resArr.length, 32, "Got bad result length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return resArr;
    }

    public static byte[] ecdsaRecover(byte[] sig, byte[] message, int recid) throws AssertFailException {
        checkArgument(sig.length == 64);
        checkArgument(message.length == 32);
        ByteBuffer byteBuff = pack(sig, message);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_recover(byteBuff, Secp256k1Context.getContext(), recid, 1);
        } finally {
            r.unlock();
        }
        byte[] resArr = retByteArray[0];
        int retVal = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();

        assertEquals(retVal, 1, "Failed return value check.");

        return resArr;
    }

    /**
     * libsecp256k1 randomize - updates the context randomization
     *
     * @param seed 32-byte random seed
     * @return true if successful
     * @throws AssertFailException in case of failure
     */
    public static synchronized boolean randomize(byte[] seed) throws AssertFailException {
        checkArgument(seed.length == 32 || seed == null);
        ByteBuffer byteBuff = pack(seed);

        w.lock();
        try {
            return secp256k1_context_randomize(byteBuff, Secp256k1Context.getContext()) == 1;
        } finally {
            w.unlock();
        }
    }

    private static native long secp256k1_ctx_clone(long context);

    private static native int secp256k1_context_randomize(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_privkey_negate(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_privkey_tweak_add(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_privkey_tweak_mul(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_pubkey_negate(ByteBuffer byteBuff, long context, int pubLen);

    private static native byte[][] secp256k1_pubkey_tweak_add(ByteBuffer byteBuff, long context, int pubLen);

    private static native byte[][] secp256k1_pubkey_tweak_mul(ByteBuffer byteBuff, long context, int pubLen);

    private static native void secp256k1_destroy_context(long context);

    private static native int secp256k1_ecdsa_verify(ByteBuffer byteBuff, long context, int sigLen, int pubLen);

    private static native byte[][] secp256k1_ecdsa_sign(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_ecdsa_sign_compact(ByteBuffer byteBuff, long context);

    private static native int secp256k1_ec_seckey_verify(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_ec_pubkey_create(ByteBuffer byteBuff, long context, int compressed);

    private static native byte[][] secp256k1_ec_pubkey_parse(ByteBuffer byteBuff, long context, int inputLen);

    private static native byte[][] secp256k1_ec_pubkey_add(ByteBuffer byteBuff, long context, int lent1, int len2);

    private static native byte[][] secp256k1_ecdh(ByteBuffer byteBuff, long context, int inputLen);

    private static native byte[][] secp256k1_ecdsa_recover(ByteBuffer byteBuff, long context, int recid, int compressed);

}
