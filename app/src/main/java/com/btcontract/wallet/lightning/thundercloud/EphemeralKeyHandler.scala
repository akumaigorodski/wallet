package com.btcontract.wallet.lightning.thundercloud

import EphemeralKeys.{selectUnusedSql, markUsedSql, newSql}
import com.btcontract.wallet.Utils.{rand, app}
import com.btcontract.wallet.helper.RichCursor
import org.bitcoinj.core.ECKey
import java.math.BigInteger
import app.LNData.db


object EphemeralKeyHandler {
  def getUnusedKey: BigInteger =
    RichCursor(db select selectUnusedSql) closeAfter { richCursor =>
      if (richCursor.size < EphemeralKeys.limit) addFreshEphemeralKeys
      // We should check if we are about to run out of fresh ephemeral keys
      // to avoid getting into racing condition where we update our bloom filter
      // and send a request with a fresh key at the same time

      richCursor.toStream.headOption map { richCursor1 =>
        db change markUsedSql(richCursor1 long EphemeralKeys.id)
        richCursor1 bigInt EphemeralKeys.privKey
      } getOrElse getUnusedKey
    }

  def addFreshEphemeralKeys = newSql(System.currentTimeMillis) match { case sqlNow =>
    for (n <- 0 to EphemeralKeys.limit) db.change(sqlNow, new ECKey(rand).getPrivKey.toString)
    app.getContentResolver.notifyChange(db sqlPath EphemeralKeys.table, null)
  }
}