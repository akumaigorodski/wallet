cmake_minimum_required (VERSION 3.4.1)

add_library(
        secp256k1

        SHARED

        src/main/cpp/secp256k1/src/secp256k1.c
        src/main/cpp/secp256k1/src/org_bitcoin_NativeSecp256k1.c
        src/main/cpp/secp256k1/src/org_bitcoin_Secp256k1Context.c
)

target_compile_options(secp256k1 PUBLIC -DHAVE_CONFIG_H)

target_include_directories(
        secp256k1

        PUBLIC

        src/main/cpp/secp256k1
        src/main/cpp/secp256k1/src
        src/main/cpp/secp256k1/include
)

target_link_libraries(
        secp256k1

        ${log-lib}
)