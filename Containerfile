FROM debian:sid-slim

RUN set -ex; \
    mkdir -p /usr/share/man/man1/; \
    apt-get update; \
    apt-get install --yes --no-install-recommends openjdk-11-jdk git wget unzip; \
    rm -rf /var/lib/apt/lists/*; \
    useradd -ms /bin/bash appuser;

USER appuser

ENV ANDROID_SDK_ROOT="/home/appuser/app/sdk" \
    ANDROID_HOME="/home/appuser/app/sdk" \
    ANDROID_NDK_HOME="/home/appuser/app/sdk/ndk/22.1.7171670/" \
    JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"

RUN set -ex; \
    mkdir -p "/home/appuser/app/sdk/licenses" "/home/appuser/app/sdk/ndk" "/home/appuser/app/simplebitcoinwallet/"; \
    printf "\n24333f8a63b6825ea9c5514f83c2829b004d1fee" > "/home/appuser/app/sdk/licenses/android-sdk-license"; \
    cd /home/appuser/app/sdk/; \
    wget https://dl.google.com/android/repository/android-ndk-r22b-linux-x86_64.zip; \
    unzip android-ndk-r22b-linux-x86_64.zip; \
    rm android-ndk-r22b-linux-x86_64.zip; \
    mv android-ndk-r22b "/home/appuser/app/sdk/ndk/22.1.7171670/"; \
    cd /home/appuser/app/simplebitcoinwallet/; \
    git clone https://github.com/btcontract/wallet; \
    cd /home/appuser/app/simplebitcoinwallet/wallet/; \
    git checkout master; \
    cd /home/appuser/app/simplebitcoinwallet/wallet/app/src/main/assets/; \
    wget https://github.com/btcontract/wallet/releases/download/2.2.14/graph.snapshot-mainnet.zlib; \
    cd /home/appuser/app/simplebitcoinwallet/wallet/;
   
WORKDIR /home/appuser/app/simplebitcoinwallet/wallet/
