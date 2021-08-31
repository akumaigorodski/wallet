Simple Bitcoin Wallet (aka SBW) is an [IMMORTAN](https://github.com/btcontract/IMMORTAN)-powered non-custodial Bitcoin wallet for Android with extensive Lightning Network support. It is fully autonomus and does not rely on any kind of centralized service (such as Google services, routing servers, special LSP nodes etc). It is the first mobile wallet which supports routing of 3rd-party Lightning payments and thus allows users to avoid private channel closings as well as earn routing fees in a non-custodial way.    

<a href="https://play.google.com/store/apps/details?id=com.btcontract.wallet"><img alt="Get it on Google Play" src="https://play.google.com/intl/en_us/badges/images/apps/en-play-badge.png" height="80pt"/></a>&nbsp;<a href="https://f-droid.org/repository/browse/?fdid=com.btcontract.wallet"><img alt="Get it on F-Droid" src="https://f-droid.org/wiki/images/5/55/F-Droid-button_get-it-on_bigger.png" height="80pt"/></a>  

## Bitcoin part roadmap

- [ ] Implement BIP157/158, let users choose between Electrum and Client Side Filters as a sync mechanism.
- [ ] Support watch-only Bitcoin wallets and import of xPub from hardware wallets.
- [ ] Allow Lightning channels to be funded from imported hardware wallets.
- [ ] Implement sweeping of funds into wallet from private keys.
- [ ] Add RBF-based boosting and cancelling for outgoing transactions.
- [ ] Add CPFP-based boosting for incoming transactions.
- [ ] Implement Taproot wallet type.
- [ ] Implement Coin Control.

## Lightning part roadmap

- [ ] Finalize and enable private hosted channels.
- [ ] Achieve complete LNURL spec support (as far as non-custodial wallet can go).
- [ ] Incorporate Rene Pickhardt and Stefan Richter [research](https://arxiv.org/abs/2107.05322) to futher improve pathfinding.
- [ ] Develop an Eclair plugin which would allow full LN nodes to easily utilize SBW routing capabilities.
- [ ] Implement Addon system for integration of 3rd party Lightning-enabled services.
- [ ] Implement UI for fine controlling of routed Lightning payments.

## Building from source

1. `$ git clone https://github.com/btcontract/wallet.git` (or download latest release source files).
2. Copy `graph.snapshot-mainnet.zlib` file into `./app/src/main/assets/` folder (snapshot file can be found in latest release).
3. `$ ./gradlew assembleRelease`
