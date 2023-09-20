package immortan.crypto

import com.sparrowwallet.hummingbird.UR
import com.sparrowwallet.hummingbird.registry.CryptoPSBT
import fr.acinq.bitcoin.Psbt.KeyPathWithMaster
import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.GenerateTxResponse
import scodec.bits.ByteVector

import scala.language.implicitConversions


object Tools {
  type Bytes = Array[Byte]
  type StringList = List[String]
  type Fiat2Btc = Map[String, Double]
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

  def prepareBip84Psbt(response: GenerateTxResponse, masterFingerprint: Long): Psbt = {
    // We ONLY support BIP84 watching wallets so all inputs have witnesses
    val psbt1 = Psbt(response.tx)

    // Provide info about inputs
    val psbt2 = response.tx.txIn.foldLeft(psbt1) { case (psbt, txIn) =>
      val parentTransaction = response.data.transactions(txIn.outPoint.txid)
      val utxoPubKey = response.data.publicScriptMap(parentTransaction.txOut(txIn.outPoint.index.toInt).publicKeyScript)
      val derivationPath = Map(KeyPathWithMaster(masterFingerprint, utxoPubKey.path) -> utxoPubKey.publicKey).map(_.swap)
      psbt.updateWitnessInputTx(parentTransaction, txIn.outPoint.index.toInt, derivationPaths = derivationPath).get
    }

    // Provide info about our change output
    response.tx.txOut.zipWithIndex.foldLeft(psbt2) { case (psbt, txOut ~ index) =>
      response.data.publicScriptChangeMap.get(txOut.publicKeyScript) map { changeKey =>
        val changeKeyPathWithMaster = KeyPathWithMaster(masterFingerprint, changeKey.path)
        val derivationPath = Map(changeKeyPathWithMaster -> changeKey.publicKey).map(_.swap)
        psbt.updateWitnessOutput(index, derivationPaths = derivationPath).get
      } getOrElse psbt
    }
  }

  def extractBip84Tx(psbt: Psbt): scala.util.Try[Transaction] = {
    // We ONLY support BIP84 watching wallets so all inputs have witnesses
    psbt.extract orElse psbt.inputs.zipWithIndex.foldLeft(psbt) { case (psbt1, input ~ index) =>
      val witness = (Script.witnessPay2wpkh _).tupled(input.partialSigs.head)
      psbt1.finalizeWitnessInput(index, witness).get
    }.extract
  }

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
