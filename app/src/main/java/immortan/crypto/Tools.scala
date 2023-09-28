package immortan.crypto

import com.sparrowwallet.hummingbird.UR
import com.sparrowwallet.hummingbird.registry.CryptoPSBT
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.Psbt.{FinalizedInput, KeyPathWithMaster}
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.GenerateTxResponse
import fr.acinq.eclair.blockchain.electrum.{ElectrumWallet, WalletSpec}
import scodec.bits.ByteVector

import scala.language.implicitConversions


object Tools {
  type Bytes = Array[Byte]
  type StringList = List[String]
  type Fiat2Btc = Map[String, Double]
  type ExtPubKeys = List[ExtendedPublicKey]
  final val SEPARATOR = " "

  def trimmed(inputText: String): String = inputText.trim.take(144)

  def none: PartialFunction[Any, Unit] = { case _ => }

  def runAnd[T](result: T)(action: Any): T = result

  implicit class Any2Some[T](underlying: T) {
    def asLeft: Left[T, Nothing] = Left(underlying)
    def asRight: Right[Nothing, T] = Right(underlying)
    def asSome: Option[T] = Some(underlying)
  }

  implicit class IterableOfTuple2[T, V](underlying: Iterable[ (T, V) ] = Nil) {
    def secondItems: Iterable[V] = underlying.map { case (_, secondItem) => secondItem }
    def firstItems: Iterable[T] = underlying.map { case (firstItem, _) => firstItem }
  }

  implicit class ThrowableOps(error: Throwable) {
    def stackTraceAsString: String = {
      val stackTraceWriter = new java.io.StringWriter
      error printStackTrace new java.io.PrintWriter(stackTraceWriter)
      stackTraceWriter.toString
    }
  }

  def prepareBip84Psbt(response: GenerateTxResponse, hwSpec: WalletSpec): Psbt = {
    require(response.usedWallets.contains(hwSpec.data.ewt), "Used wallets must contain HW")
    val specs = response.usedWallets.map(_.xPub).map(ElectrumWallet.specs)
    val psbt1 = Psbt(response.tx)

    val psbt2 = response.tx.txIn.zipWithIndex.zip(specs).foldLeft(psbt1) { case (psbt, txIn ~ index ~ spec) =>
      // For now this will accept a tx with mixed inputs where at most one wallet is BIP84 hardware one
      val parentTransaction = spec.data.transactions(txIn.outPoint.txid)

      if (spec == hwSpec) {
        val utxoPubKey = spec.data.publicScriptMap(parentTransaction.txOut(txIn.outPoint.index.toInt).publicKeyScript)
        val derivationPath = utxoPubKey.publicKey -> KeyPathWithMaster(hwSpec.info.core.masterFingerprint.get, utxoPubKey.path)
        psbt.updateWitnessInputTx(parentTransaction, derivationPaths = Map(derivationPath), outputIndex = txIn.outPoint.index.toInt).get
      } else if (spec.info.core.walletType == ElectrumWallet.BIP84) {
        // This comes from local signing BIP84 wallet so we can finalize this input right away
        val updated = psbt.updateWitnessInputTx(parentTransaction, txIn.outPoint.index.toInt).get
        updated.finalizeWitnessInput(index, txIn.witness).get
      } else {
        // This comes from local signing non-witness wallet so we can finalize this input right away
        val updated = psbt.updateNonWitnessInput(parentTransaction, txIn.outPoint.index.toInt).get
        updated.finalizeNonWitnessInput(index, Script parse txIn.signatureScript).get
      }
    }

    // Provide info about our change output
    response.tx.txOut.zipWithIndex.foldLeft(psbt2) { case (psbt, txOut ~ index) =>
      hwSpec.data.publicScriptChangeMap.get(key = txOut.publicKeyScript) map { changeKey =>
        val changeKeyPathWithMaster = KeyPathWithMaster(hwSpec.info.core.masterFingerprint.get, changeKey.path)
        val derivationPath = Map(changeKey.publicKey -> changeKeyPathWithMaster)
        psbt.updateWitnessOutput(index, derivationPaths = derivationPath).get
      } getOrElse psbt
    }
  }

  def extractBip84Tx(psbt: Psbt): scala.util.Try[Transaction] =
    psbt.extract orElse psbt.inputs.zipWithIndex.foldLeft(psbt) {
      case psbt1 ~ Tuple2(_: FinalizedInput, _) =>
        // Came from local signing wallet
        psbt1

      case psbt1 ~ Tuple2(input, index) =>
        val (pubKey, sig) = input.partialSigs.head
        val witness = Script.witnessPay2wpkh(pubKey, sig)
        psbt1.finalizeWitnessInput(index, witness).get
    }.extract

  def obtainPsbt(ur: UR): scala.util.Try[Psbt] = scala.util.Try {
    val rawPsbt = ur.decodeFromRegistry.asInstanceOf[CryptoPSBT]
    ByteVector.view(rawPsbt.getPsbt)
  } flatMap Psbt.read

  object ~ {
    // Useful for matching nested Tuple2 with less noise
    def unapply[A, B](t2: (A, B) /* Got a tuple */) = Some(t2)
  }
}

trait CanBeShutDown {
  def becomeShutDown: Unit
}
