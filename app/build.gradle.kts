plugins {
    alias(libs.plugins.androidApplication)
    id("org.barfuin.gradle.taskinfo") version "2.2.0"
}

apply(plugin = "com.soundcorset.scala-android")

android {
    namespace = "com.btcontract.wallet"
    compileSdk = 34

    defaultConfig {
        applicationId = "com.btcontract.wallet"
        versionName = "2.5.9"
        versionCode = 110
        targetSdk = 34
        minSdk = 28
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt")
            )
        }
        debug {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt")
            )
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }

    packaging {
        resources {
            excludes += "META-INF/**"
        }
    }
}

dependencies {
    implementation(libs.scala.library)

    // Android-specific
    implementation("androidx.appcompat:appcompat:1.3.1")
    implementation("com.cottacush:CurrencyEditText:0.0.10")
    implementation("com.google.android.material:material:1.4.0")
    implementation("com.journeyapps:zxing-android-embedded:4.2.0")
    implementation("com.google.android.play:review:2.0.1")
    implementation("androidx.multidex:multidex:2.0.1")

    // Immortan
    implementation("fr.acinq.secp256k1:secp256k1-kmp-jni-android:0.5.2")
    implementation("org.scala-lang.modules:scala-parser-combinators_2.11:2.1.0")
    implementation("com.softwaremill.quicklens:quicklens_2.11:1.7.5")
    implementation("com.typesafe.akka:akka-actor_2.11:2.3.14")
    implementation("org.scodec:scodec-core_2.11:1.11.4")
    implementation("io.reactivex:rxscala_2.11:0.27.0")

    implementation("org.json4s:json4s-native_2.11:3.6.7")
    implementation("io.spray:spray-json_2.11:1.3.6")

    implementation("org.bouncycastle:bcprov-jdk15to18:1.68")
    implementation("com.google.guava:guava:29.0-android")
    implementation("commons-codec:commons-codec:1.11")
    implementation("io.netty:netty-all:4.1.42.Final")

    // Built-in Tor
    implementation("com.squareup.okhttp3:okhttp:4.9.1")
    implementation("info.guardianproject:tor-android:0.4.7.11")
    implementation("info.guardianproject:jtorctl:0.4.5.7")
}
