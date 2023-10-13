package immortan.crypto

import com.sparrowwallet.hummingbird.UR
import com.sparrowwallet.hummingbird.registry.CryptoPSBT
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin.Psbt.KeyPathWithMaster
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
    def asVector: Vector[T] = Vector(underlying)
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
    require(response.usedWallets.contains(hwSpec.data.keys.ewt), "Used wallets must contain a hardware wallet")
    val psbt1 = response.tx.txIn.zip(response.usedWallets).foldLeft(Psbt apply response.tx) { case (psbt, txIn \ ewt) =>
      // For now this will accept a tx with mixed inputs where at most one origin can be BIP84 hardware wallet

      val idx = txIn.outPoint.index.toInt
      val spec = ElectrumWallet.specs(ewt.xPub)
      val parentTx = spec.data.transactions(txIn.outPoint.txid)

      if (spec.info.core.walletType == ElectrumWallet.BIP84 && spec == hwSpec) {
        val utxoPubKey = spec.data.keys.publicScriptMap(parentTx.txOut(idx).publicKeyScript)
        val keyPathWithMaster = KeyPathWithMaster(hwSpec.info.core.masterFingerprint.get, utxoPubKey.path)
        psbt.updateWitnessInputTx(parentTx, derivationPaths = Map(utxoPubKey.publicKey -> keyPathWithMaster), outputIndex = idx).get
      } else if (spec.info.core.walletType == ElectrumWallet.BIP84) psbt.updateWitnessInputTx(parentTx, outputIndex = idx).get
      else psbt.updateNonWitnessInput(parentTx, outputIndex = idx).get
    }

    // Provide info about our change output
    response.tx.txOut.zipWithIndex.foldLeft(psbt1) { case (psbt, txOut \ index) =>
      hwSpec.data.keys.publicScriptChangeMap.get(key = txOut.publicKeyScript) map { changeKey =>
        val keyPathWithMaster = KeyPathWithMaster(hwSpec.info.core.masterFingerprint.get, changeKey.path)
        psbt.updateWitnessOutput(derivationPaths = Map(changeKey.publicKey -> keyPathWithMaster), outputIndex = index).get
      } getOrElse psbt
    }
  }

  def extractTx(response: GenerateTxResponse, hwSpec: WalletSpec, psbt: Psbt): scala.util.Try[Transaction] = {
    val specs = for (ewt <- response.usedWallets) yield ElectrumWallet.specs(ewt.xPub)

    response.tx.txIn.zipWithIndex.zip(psbt.inputs).zip(specs).foldLeft(psbt) {
      case (psbt1, _ \ index \ input \ spec) if spec.info.core.walletType == ElectrumWallet.BIP84 && spec == hwSpec =>
        // This input has been signed by hardware wallet, related PSBT input must contain a signature as first map entry
        val witness = (Script.witnessPay2wpkh _).tupled(input.partialSigs.head)
        psbt1.finalizeWitnessInput(index, witness).get

      case (psbt1, txIn \ index \ _ \ spec) if spec.info.core.walletType == ElectrumWallet.BIP84 =>
        // This input has been signed by BIP84 local signing wallet, take witness from tx input
        psbt1.finalizeWitnessInput(index, txIn.witness).get

      case (psbt1, txIn \ index \ _ \ _) =>
        // This input has been signed by legacy local signing wallet, take scriptSig from tx input
        psbt1.finalizeNonWitnessInput(index, scriptSig = Script parse txIn.signatureScript).get
    }.extract
  }

  def obtainPsbt(ur: UR): scala.util.Try[Psbt] = scala.util.Try {
    val rawPsbt = ur.decodeFromRegistry.asInstanceOf[CryptoPSBT]
    ByteVector.view(rawPsbt.getPsbt)
  } flatMap Psbt.read

  object \ {
    // Useful for matching nested Tuple2 with less noise
    def unapply[A, B](t2: (A, B) /* Got a tuple */) = Some(t2)
  }
}

trait CanBeShutDown {
  def becomeShutDown: Unit
}
