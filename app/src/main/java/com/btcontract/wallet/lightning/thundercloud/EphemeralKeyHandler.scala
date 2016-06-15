package com.btcontract.wallet.lightning.thundercloud

import EphemeralKeys.{selectUnusedSql, markUsedSql, newSql}
import com.btcontract.wallet.Utils.{rand, app}
import com.btcontract.wallet.helper.RichCursor
import org.bitcoinj.core.ECKey
import java.math.BigInteger
import app.LNData.db


object EphemeralKeyHandler {
  // If less than <limit> unused keys left then add new ones
  // Adding keys before recursive call to guarantee we have some keys on next run

  def choose: BigInteger =
    new RichCursor(db select selectUnusedSql) closeAfter { rc =>
      if (rc.c.getCount < EphemeralKeys.limit) addUnusedKeysWhenNotEnough
      if (rc.iterator.hasNext) markUsedAndReturn(rc.iterator.next) else choose
    }

  def markUsedAndReturn(rc: RichCursor) = {
    db change markUsedSql(rc long EphemeralKeys.id)
    rc bigInt EphemeralKeys.privKey
  }

  def addUnusedKeysWhenNotEnough = {
    val insert = newSql(System.currentTimeMillis)
    val keys = Stream continually new ECKey(rand).getPrivKey
    for (pk <- keys take EphemeralKeys.plus) db.change(insert, pk.toString)
    app.getContentResolver.notifyChange(db sqlPath EphemeralKeys.table, null)
  }
}
