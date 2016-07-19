package com.btcontract.wallet.lightning.thundercloud

import EphemeralKeys.{selectUnusedSql, markUsedSql, newSql}
import com.btcontract.wallet.Utils.{rand, app}
import System.{currentTimeMillis => now}

import com.btcontract.wallet.helper.RichCursor
import org.bitcoinj.core.ECKey
import java.math.BigInteger
import app.LNData.db


object EphemeralKeyHandler {
  def getUnusedKey: BigInteger =
    RichCursor(db select selectUnusedSql) closeAfter { richCursor =>
      richCursor.toStream.headOption map markUsedAndReturn getOrElse {
        for (n <- 0 to 25) db.change(newSql(now), new ECKey(rand).getPrivKey.toString)
        app.getContentResolver.notifyChange(db sqlPath EphemeralKeys.table, null)
        getUnusedKey
      }
    }

  def markUsedAndReturn(richCursor: RichCursor) = {
    db change markUsedSql(richCursor long EphemeralKeys.id)
    richCursor bigInt EphemeralKeys.privKey
  }
}
