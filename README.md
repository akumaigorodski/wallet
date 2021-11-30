Simple Bitcoin Wallet (aka SBW) is an [IMMORTAN](https://github.com/btcontract/IMMORTAN)-powered non-custodial Bitcoin wallet for Android with extensive Lightning Network support. It is fully autonomus and does not rely on any kind of centralized service (such as Google services, routing servers, special LSP nodes etc). It is the first mobile wallet which supports routing of 3rd-party Lightning payments and thus allows users to avoid private channel closings as well as earn routing fees in a non-custodial way.    

<a href="https://play.google.com/store/apps/details?id=com.btcontract.wallet"><img alt="Get it on Google Play" src="https://play.google.com/intl/en_us/badges/images/apps/en-play-badge.png" height="80pt"/></a>&nbsp;<a href="https://f-droid.org/repository/browse/?fdid=com.btcontract.wallet"><img alt="Get it on F-Droid" src="https://f-droid.org/wiki/images/5/55/F-Droid-button_get-it-on_bigger.png" height="80pt"/></a>  

## Bitcoin part roadmap

- [ ] Implement BIP157/158, let users choose between Electrum and Client Side Filters as a sync mechanism.
- [x] Support watch-only Bitcoin wallets and import of xPub from hardware wallets.
- [x] Allow Lightning channels to be funded from imported hardware wallets.
- [x] Add RBF-based boosting and cancelling for outgoing transactions.
- [x] Add CPFP-based boosting for incoming transactions.
- [ ] Implement Taproot wallet type.
- [x] Implement Coin Control.

## Lightning part roadmap

- [x] Finalize and enable private hosted channels.
- [x] Achieve complete LNURL spec support (as far as non-custodial wallet can go).
- [ ] Develop an Eclair plugin which would allow full LN nodes to easily utilize SBW routing capabilities.
- [ ] Implement UI for fine controlling of routed Lightning payments.

## Building from source

```
git clone https://github.com/btcontract/wallet.git
cd wallet
git checkout 2.4.21
podman build -t sbw .
podman run -v $PWD:/app/simplebitcoinwallet/wallet:z sbw
```

### Signing with your self-signed certificate

Install Android SDK, create a `keystore.jks` using `keytool`.

```
$ <Android SDK dir>/build-tools/<version>/zipalign -v 4 app/build/outputs/apk/release/SBW-2.4.21.apk app/build/outputs/apk/release/SBW-2.4.21-aligned.apk

$ <Android SDK dir>/build-tools/<version>/apksigner sign --ks <path to keystore.jks> --ks-key-alias <signing key alias> --v1-signing-enabled true --v2-signing-enabled true app/build/outputs/apk/release/SBW-2.4.21-aligned.apk
```

## Verification with `apksigner`

```
$ '<Android SDK dir>/build-tools/<version>/apksigner' verify --print-certs --verbose SBW-2.4.21.apk
```

Output should contain the following info:

```
Verifies
Verified using v1 scheme (JAR signing): true
Verified using v2 scheme (APK Signature Scheme v2): true
Number of signers: 1
Signer #1 certificate DN: CN=Bitcoins wallet developer
Signer #1 certificate SHA-256 digest: dca2c3527ec7f7c0e38c0353278e7a5674cfa6e4b7556510ff05f60073ca338a
Signer #1 certificate SHA-1 digest: 14659e7de5a71f2608bf4a889c0f8d043147e203
Signer #1 certificate MD5 digest: e3102232a4754705c8917710765b9635
Signer #1 key algorithm: RSA
Signer #1 key size (bits): 2048
Signer #1 public key SHA-256 digest: dc97f0f2e34167015914600d8fa748f908d578bcedb79664d010de3c9bdebf13
Signer #1 public key SHA-1 digest: c4400469d5ad807dd9394785f1fa95003588a091
Signer #1 public key MD5 digest: e4e1f847e0cb0a9703dc4f9323fd6d87
```

### Project sponsors

<table>
  <tbody>
    <tr>
      <td align="center" valign="middle">
        <a href="https://lnbig.com/" target="_blank">
          <img width="146px" src="https://i.imgur.com/W4A92Ym.png">
        </a>
      </td>
    </tr>
  </tbody>
</table>
