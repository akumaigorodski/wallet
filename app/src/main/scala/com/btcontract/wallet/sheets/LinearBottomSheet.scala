package com.btcontract.wallet.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.LinearLayout
import com.btcontract.wallet.BaseActivity
import com.google.android.material.bottomsheet.BottomSheetDialogFragment


class LinearBottomSheet(viewRes: Int, host: BaseActivity) extends BottomSheetDialogFragment {
  val view: LinearLayout = host.getLayoutInflater.inflate(viewRes, null).asInstanceOf[LinearLayout]
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = view
}