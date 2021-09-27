package com.btcontract.wallettest.utils

import androidx.biometric.{BiometricManager, BiometricPrompt}
import com.btcontract.wallettest.BaseActivity.StringOps
import com.btcontract.wallettest.BaseActivity
import androidx.core.content.ContextCompat
import com.btcontract.wallettest.R
import android.view.View


abstract class BiometricAuth(view: View, host: BaseActivity) {
  val biometricManager: BiometricManager = BiometricManager.from(host)

  def onAuthSucceeded: Unit
  def onHardwareUnavailable: Unit
  def onCanAuthenticate: Unit
  def onNoneEnrolled: Unit
  def onNoHardware: Unit

  def checkAuth: Unit = biometricManager.canAuthenticate match {
    case BiometricManager.BIOMETRIC_ERROR_NO_HARDWARE => onNoHardware
    case BiometricManager.BIOMETRIC_ERROR_HW_UNAVAILABLE => onHardwareUnavailable
    case BiometricManager.BIOMETRIC_ERROR_NONE_ENROLLED => onNoneEnrolled
    case BiometricManager.BIOMETRIC_SUCCESS => onCanAuthenticate
    case _ => onHardwareUnavailable
  }

  def callAuthDialog: Unit = {
    val promptInfo: BiometricPrompt.PromptInfo =
      (new BiometricPrompt.PromptInfo.Builder)
        .setTitle(host getString R.string.settings_auth_title)
        .setDeviceCredentialAllowed(true)
        .build

    val callback: BiometricPrompt.AuthenticationCallback = new BiometricPrompt.AuthenticationCallback {
      override def onAuthenticationSucceeded(result: BiometricPrompt.AuthenticationResult): Unit = {
        super.onAuthenticationSucceeded(result)
        onAuthSucceeded
      }

      override def onAuthenticationError(errorCode: Int, errString: CharSequence): Unit = {
        val message = host.getString(R.string.settings_auth_error).format(errString, errorCode).html
        host.snack(view, message, R.string.dialog_ok, _.dismiss)
        super.onAuthenticationError(errorCode, errString)
      }

      override def onAuthenticationFailed: Unit =
        super.onAuthenticationFailed
    }

    val executor = ContextCompat.getMainExecutor(host)
    val biometricPrompt = new BiometricPrompt(host, executor, callback)
    biometricPrompt.authenticate(promptInfo)
  }
}
