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
  val proofCodec: Codec[GetMerkleResponse] = (
    ("txid" | bytes32) ::
      ("merkle" | listOfN(uint16, bytes32)) ::
      ("block_height" | uint24) ::
      ("pos" | uint24) ::
      ("context_opt" | provide(Option.empty[Any]))).as[GetMerkleResponse]

  def serializeMerkleProof(proof: GetMerkleResponse): Array[Byte] = proofCodec.encode(proof).require.toByteArray

  def deserializeMerkleProof(bin: Array[Byte]): GetMerkleResponse = proofCodec.decode(BitVector(bin)).require.value

  val statusListCodec: Codec[List[(ByteVector32, String)]] = listOfN(uint16, bytes32 ~ cstring)

  val statusCodec: Codec[Map[ByteVector32, String]] = Codec[Map[ByteVector32, String]](
    (map: Map[ByteVector32, String]) => statusListCodec.encode(map.toList),
    (wire: BitVector) => statusListCodec.decode(wire).map(_.map(_.toMap))
  )

  val heightsListCodec: Codec[List[(ByteVector32, Int)]] = listOfN(uint16, bytes32 ~ int32)

  val heightsCodec: Codec[Map[ByteVector32, Int]] = Codec[Map[ByteVector32, Int]](
    (map: Map[ByteVector32, Int]) => heightsListCodec.encode(map.toList),
    (wire: BitVector) => heightsListCodec.decode(wire).map(_.map(_.toMap))
  )

  val transactionListCodec: Codec[List[(ByteVector32, Transaction)]] = listOfN(uint16, bytes32 ~ txCodec)

  val transactionsCodec: Codec[Map[ByteVector32, Transaction]] = Codec[Map[ByteVector32, Transaction]](
    (map: Map[ByteVector32, Transaction]) => transactionListCodec.encode(map.toList),
    (wire: BitVector) => transactionListCodec.decode(wire).map(_.map(_.toMap))
  )

  val transactionHistoryItemCodec: Codec[ElectrumClient.TransactionHistoryItem] = (
    ("height" | int32) :: ("tx_hash" | bytes32)).as[ElectrumClient.TransactionHistoryItem]

  val seqOfTransactionHistoryItemCodec: Codec[List[TransactionHistoryItem]] = listOfN[TransactionHistoryItem](uint16, transactionHistoryItemCodec)

  val historyListCodec: Codec[List[(ByteVector32, ElectrumWallet.TransactionHistoryItemList)]] =
    listOfN[(ByteVector32, ElectrumWallet.TransactionHistoryItemList)](uint16, bytes32 ~ seqOfTransactionHistoryItemCodec)

  val historyCodec: Codec[Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]] = Codec[Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]](
    (map: Map[ByteVector32, ElectrumWallet.TransactionHistoryItemList]) => historyListCodec.encode(map.toList),
    (wire: BitVector) => historyListCodec.decode(wire).map(_.map(_.toMap))
  )

  val proofsListCodec: Codec[List[(ByteVector32, GetMerkleResponse)]] = listOfN(uint16, bytes32 ~ proofCodec)

  val proofsCodec: Codec[Map[ByteVector32, GetMerkleResponse]] = Codec[Map[ByteVector32, GetMerkleResponse]](
    (map: Map[ByteVector32, GetMerkleResponse]) => proofsListCodec.encode(map.toList),
    (wire: BitVector) => proofsListCodec.decode(wire).map(_.map(_.toMap))
  )

  /**
    * change this value
    * -if the new codec is incompatible with the old one
    * - OR if you want to force a full sync from Electrum servers
    */
  val version = 0x0000

  val persistentDataCodec: Codec[PersistentData] = {
    ("version" | constant(BitVector.fromInt(version))) ::
      ("accountKeysCount" | int32) ::
      ("changeKeysCount" | int32) ::
      ("status" | statusCodec) ::
      ("transactions" | transactionsCodec) ::
      ("heights" | heightsCodec) ::
      ("history" | historyCodec) ::
      ("proofs" | proofsCodec) ::
      ("pendingTransactions" | listOfN(uint16, txCodec))
  }.as[PersistentData]
}