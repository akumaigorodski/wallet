cmake_minimum_required (VERSION 3.4.1)

set(CMAKE_BUILD_TYPE 'Release')
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -fPIC -Os")

if(ANDROID_ABI STREQUAL "armeabi-v7a")
    set(CMAKE_C_FLAGS "${ANDROID_CXX_FLAGS} ${CMAKE_C_FLAGS} -mfloat-abi=softfp -mfpu=neon -mthumb")
elseif(ANDROID_ABI STREQUAL "arm64-v8a")
    set(CMAKE_C_FLAGS "${ANDROID_CXX_FLAGS} ${CMAKE_C_FLAGS} -flax-vector-conversions")
endif()


find_library(log-lib
        log)

add_library(wallycore SHARED

        src/address.c
        src/aes.c
        src/base58.c
        src/bech32.c
        src/bip32.c
        src/bip38.c
        src/bip39.c
        src/ecdh.c
        src/elements.c
        src/hex.c
        src/hmac.c
        src/internal.c
        src/mnemonic.c
        src/pbkdf2.c
        src/scrypt.c
        src/sign.c
        src/transaction.c
        src/script.c
        src/symmetric.c
        src/wif.c
        src/wordlist.c
        src/ccan/ccan/crypto/ripemd160/ripemd160.c
        src/ccan/ccan/crypto/sha256/sha256.c
        src/ccan/ccan/crypto/sha512/sha512.c
        src/ccan/ccan/str/hex/hex.c

        src/swig_java/swig_java_wrap.c
)

target_include_directories(wallycore
    PUBLIC
        .
        include
        src
        src/ccan
        src/secp256k1
        src/secp256k1/src
        src/secp256k1/include
)

target_compile_definitions(wallycore PRIVATE SECP256K1_BUILD SWIG_JAVA_BUILD)
target_link_libraries(wallycore secp256k1 android ${log-lib})

add_library(secp256k1 SHARED

        src/secp256k1/src/secp256k1.c
        src/secp256k1/src/org_bitcoin_NativeSecp256k1.c
        src/secp256k1/src/org_bitcoin_Secp256k1Context.c
        )

target_include_directories(secp256k1
        PUBLIC
        src/secp256k1
        src/secp256k1/src
        src/secp256k1/include
        )

target_compile_options(secp256k1
        PUBLIC
        -DHAVE_CONFIG_H
        )

target_compile_definitions(secp256k1
        PUBLIC
        -DHAVE_CONFIG_H
        )

target_link_libraries(secp256k1 c android log)