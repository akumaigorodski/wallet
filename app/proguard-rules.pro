-dontoptimize
-dontobfuscate
-dontpreverify
-ignorewarnings

-dontwarn scala.**

-keep class scala.collection.SeqLike { public protected *; }

-keep fr.acinq.eclair.blockchain.rpc.** { *; }

-keepattributes Signature,*Annotation*

-dontwarn javax.annotation.**
