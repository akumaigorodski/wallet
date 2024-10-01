package com.btcontract.wallet.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.ListView
import com.btcontract.wallet.ChoiceReceiver
import com.btcontract.wallet.utils.OnListItemClickListener
import com.google.android.material.bottomsheet.BottomSheetDialogFragment


class ChoiceBottomSheet(list: ListView, tag: AnyRef, host: ChoiceReceiver) extends BottomSheetDialogFragment {
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = list

  override def onViewCreated(view: View, state: Bundle): Unit = {
    view.setBackgroundResource(com.btcontract.wallet.R.color.chip_default_text_color)

    list setOnItemClickListener new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        host.onChoiceMade(tag, itemPosition)
        dismiss
      }
    }
  }
}
