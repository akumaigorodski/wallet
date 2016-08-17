package com.btcontract.wallet.lightning.lncloud

import EphemeralKeys.{selectUnusedSql, markUsedSql, newSql}
import com.btcontract.wallet.Utils.{rand, app}
import com.btcontract.wallet.helper.RichCursor
import org.bitcoinj.core.ECKey
import java.math.BigInteger


object EphemeralKeyHandler {
  def getUnusedKey: BigInteger =
    RichCursor(app.LNData.db select selectUnusedSql) closeAfter { richCursor =>
      // We should check if we are *about* to run out of fresh ephemeral keys to avoid getting
      // into racing condition where we update our bloom filter and send a request
      // with a fresh key at the same time

      // `c.getCount` instead of `size` because calling `size` will exhaust an underlying cursor
      if (richCursor.c.getCount < EphemeralKeys.limit) newSql(System.currentTimeMillis) match { case sqlNow =>
        for (n <- 0 to EphemeralKeys.limit) app.LNData.db.change(sqlNow, new ECKey(rand).getPrivKey.toString)
        app.getContentResolver.notifyChange(app.LNData.db sqlPath EphemeralKeys.table, null)
      }

      richCursor.toStream.headOption map { case richCursor1 =>
        app.LNData.db change markUsedSql(richCursor1 long EphemeralKeys.id)
        richCursor1 bigInt EphemeralKeys.privKey
      } getOrElse getUnusedKey
    }
}