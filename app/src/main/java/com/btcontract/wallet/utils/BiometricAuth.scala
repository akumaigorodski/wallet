package com.btcontract.wallet.utils

import android.view.View
import androidx.biometric.{BiometricManager, BiometricPrompt}
import androidx.core.content.ContextCompat
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.{BaseActivity, R}
import com.google.android.material.snackbar.Snackbar


abstract class BiometricAuth(view: View, host: BaseActivity, onErrorSnackOk: Snackbar => Unit) {
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
    val promptInfo: BiometricPrompt.PromptInfo = {
      val builder = new BiometricPrompt.PromptInfo.Builder
      val title = host getString R.string.settings_auth_title
      builder.setDeviceCredentialAllowed(true).setTitle(title).build
    }

    val callback: BiometricPrompt.AuthenticationCallback = new BiometricPrompt.AuthenticationCallback {
      override def onAuthenticationSucceeded(result: BiometricPrompt.AuthenticationResult): Unit = {
        super.onAuthenticationSucceeded(result)
        onAuthSucceeded
      }

      override def onAuthenticationError(errorCode: Int, errString: CharSequence): Unit = {
        val message = host.getString(R.string.settings_auth_error).format(errString).html
        host.snack(view, message, R.string.dialog_ok, onErrorSnackOk)
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
