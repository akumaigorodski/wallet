package fr.acinq.eclair.blockchain.electrum.db.sqlite

import scodec.codecs._
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.ChannelCodecs._
import fr.acinq.bitcoin.{ByteVector32, Transaction}
import fr.acinq.eclair.blockchain.electrum.{ElectrumClient, ElectrumWallet}
import fr.acinq.eclair.blockchain.electrum.ElectrumClient.{GetMerkleResponse, TransactionHistoryItem}
import fr.acinq.eclair.blockchain.electrum.PersistentData
import scodec.bits.BitVector
import scodec.Codec


object SqliteWalletDb {
  private val anyOpt = Option.empty[Any]

  val proofCodec = {
    (bytes32 withContext "txid") ::
      (listOfN(uint16, bytes32) withContext "merkle") ::
      (uint24 withContext "blockHeight") ::
      (uint24 withContext "pos") ::
      (provide(anyOpt) withContext "contextOpt")
  }.as[GetMerkleResponse]

  val statusCodec: Codec[Map[ByteVector32, String]] = Codec[Map[ByteVector32, String]](
    (runtimeMap: Map[ByteVector32, String]) => listOfN(uint16, bytes32 ~ cstring).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ cstring).decode(wire).map(_.map(_.toMap))
  )

  val transactionsCodec: Codec[Map[ByteVector32, Transaction]] = Codec[Map[ByteVector32, Transaction]](
    (runtimeMap: Map[ByteVector32, Transaction]) => listOfN(uint16, bytes32 ~ txCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ txCodec).decode(wire).map(_.map(_.toMap))
  )

  val transactionHistoryItemCodec = {
    (int32 withContext "height") ::
      (bytes32 withContext "txHash")
  }.as[ElectrumClient.TransactionHistoryItem]

  val seqOfTransactionHistoryItemCodec = listOfN[TransactionHistoryItem](uint16, transactionHistoryItemCodec)

  val historyCodec: Codec[Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]] = Codec[Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]](
    (runtimeMap: Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]) => listOfN(uint16, bytes32 ~ seqOfTransactionHistoryItemCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ seqOfTransactionHistoryItemCodec).decode(wire).map(_.map(_.toMap))
  )

  val proofsCodec: Codec[Map[ByteVector32, GetMerkleResponse]] = Codec[Map[ByteVector32, GetMerkleResponse]](
    (runtimeMap: Map[ByteVector32, GetMerkleResponse]) => listOfN(uint16, bytes32 ~ proofCodec).encode(runtimeMap.toList),
    (wire: BitVector) => listOfN(uint16, bytes32 ~ proofCodec).decode(wire).map(_.map(_.toMap))
  )

  val version = 0x0000

  val persistentDataCodec: Codec[PersistentData] = {
    (constant(BitVector fromInt version) withContext "version") ::
      (int32 withContext "accountKeysCount") ::
      (int32 withContext "changeKeysCount") ::
      (statusCodec withContext "status") ::
      (transactionsCodec withContext "transactions") ::
      (setCodec(bytes32) withContext "overriddenPendingTxids") ::
      (historyCodec withContext "history") ::
      (proofsCodec withContext "proofs") ::
      (listOfN(uint16, txCodec) withContext "pendingTransactions")
  }.as[PersistentData]
}
