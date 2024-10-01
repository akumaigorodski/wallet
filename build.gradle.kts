buildscript {
    repositories {
        mavenLocal()
    }
    dependencies {
        classpath(libs.scala.android.plugin)
    }
}

plugins {
    alias(libs.plugins.androidApplication) apply false
}