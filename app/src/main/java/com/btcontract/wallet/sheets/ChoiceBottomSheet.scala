package com.btcontract.wallet.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.ListView
import com.btcontract.wallet.ChoiceReceiver
import com.btcontract.wallet.utils.OnListItemClickListener
import com.google.android.material.bottomsheet.BottomSheetDialogFragment


class BaseChoiceBottomSheet(list: ListView) extends BottomSheetDialogFragment { me =>
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = list
}

class ChoiceBottomSheet(list: ListView, tag: AnyRef, host: ChoiceReceiver) extends BaseChoiceBottomSheet(list) { me =>
  override def onViewCreated(view: View, state: Bundle): Unit =
    list setOnItemClickListener new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        host.onChoiceMade(tag, itemPosition)
        dismiss
      }
    }
}
