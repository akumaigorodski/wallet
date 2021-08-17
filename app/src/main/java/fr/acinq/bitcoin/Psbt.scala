package fr.acinq.bitcoin

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.DeterministicWallet.{ExtendedPublicKey, KeyPath}
import scodec.bits.{ByteVector, HexStringSyntax}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.ByteOrder
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * A partially signed bitcoin transaction: see https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki.
 *
 * @param global  global psbt data containing the transaction to be signed.
 * @param inputs  signing data for each input of the transaction to be signed (order matches the unsigned tx).
 * @param outputs signing data for each output of the transaction to be signed (order matches the unsigned tx).
 */
case class Psbt(global: Psbt.Global, inputs: Seq[Psbt.PartiallySignedInput], outputs: Seq[Psbt.PartiallySignedOutput]) {

  import Psbt._

  require(global.tx.txIn.length == inputs.length, "there must be one partially signed input per input of the unsigned tx")
  require(global.tx.txOut.length == outputs.length, "there must be one partially signed output per output of the unsigned tx")

  /**
   * Implements the PSBT updater role; adds information about a given UTXO.
   * Note that we always fill the nonWitnessUtxo (see https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#cite_note-7).
   *
   * @param inputTx         transaction containing the UTXO.
   * @param outputIndex     index of the UTXO in the inputTx.
   * @param redeemScript    redeem script if known and applicable.
   * @param witnessScript   witness script if known and applicable.
   * @param sighashType     sighash type if one should be specified.
   * @param derivationPaths derivation paths for keys used by this UTXO.
   * @return psbt with the matching input updated.
   */
  def update(inputTx: Transaction,
             outputIndex: Int,
             redeemScript: Option[Seq[ScriptElt]] = None,
             witnessScript: Option[Seq[ScriptElt]] = None,
             sighashType: Option[Int] = None,
             derivationPaths: Map[PublicKey, KeyPathWithMaster] = Map.empty): Try[Psbt] = Try {
    require(outputIndex < inputTx.txOut.size, "output index must exist in the input tx")
    val outpoint = OutPoint(inputTx, outputIndex)
    val inputIndex = global.tx.txIn.indexWhere(_.outPoint == outpoint)
    require(inputIndex >= 0, "psbt transaction does not spend the provided outpoint")
    val input = inputs(inputIndex)
    val withUtxo = if (witnessScript.nonEmpty) {
      input.copy(
        witnessUtxo = Some(inputTx.txOut(outputIndex)), redeemScript = redeemScript.orElse(input.redeemScript), witnessScript = witnessScript,
        nonWitnessUtxo = Some(inputTx)
      )
    } else {
      input.copy(nonWitnessUtxo = Some(inputTx), redeemScript = redeemScript.orElse(input.redeemScript))
    }
    val withSigHashAndDerivation = withUtxo.copy(
      sighashType = sighashType.orElse(input.sighashType),
      derivationPaths = input.derivationPaths ++ derivationPaths
    )
    this.copy(inputs = inputs.updated(inputIndex, withSigHashAndDerivation))
  }

  /**
   * Implements the PSBT signer role: sign a given input.
   * The caller needs to carefully verify that it wants to spend that input, and that the unsigned transaction matches
   * what it expects.
   *
   * @param priv       private key used to sign the input.
   * @param inputIndex index of the input that should be signed.
   * @return the psbt with a partial signature added (other inputs will not be modified).
   */
  def sign(priv: PrivateKey, inputIndex: Int): Try[Psbt] = Try {
    require(inputIndex < inputs.length, "input index must exist in the input tx")
    val input = inputs(inputIndex)
    Psbt.sign(priv, inputIndex, input, global) match {
      case Success(signedInput) => this.copy(inputs = inputs.updated(inputIndex, signedInput))
      case Failure(ex) => return Failure(ex)
    }
  }

  /**
   * Implements the PSBT finalizer role: finalizes a given non-witness input.
   * This will clear all fields from the input except the utxo, scriptSig and unknown entries.
   *
   * @param inputIndex index of the input that should be finalized.
   * @param scriptSig  signature script.
   * @return a psbt with the given input finalized.
   */
  def finalize(inputIndex: Int, scriptSig: Seq[ScriptElt]): Try[Psbt] = {
    if (inputIndex >= inputs.length) {
      Failure(new IllegalArgumentException("input index must exist in the input tx"))
    } else {
      inputs(inputIndex) match {
        case input if input.nonWitnessUtxo.isEmpty => Failure(new IllegalArgumentException("cannot finalize: non-witness utxo is missing"))
        case input =>
          val finalizedInput = input.copy(
            sighashType = None,
            partialSigs = Map.empty,
            derivationPaths = Map.empty,
            redeemScript = None,
            witnessScript = None,
            scriptSig = Some(scriptSig)
          )
          Success(this.copy(inputs = this.inputs.updated(inputIndex, finalizedInput)))
      }
    }
  }

  /**
   * Implements the PSBT finalizer role: finalizes a given witness input.
   * This will clear all fields from the input except the utxo, scriptSig, scriptWitness and unknown entries.
   *
   * @param inputIndex    index of the input that should be finalized.
   * @param scriptWitness witness script.
   * @return a psbt with the given input finalized.
   */
  def finalize(inputIndex: Int, scriptWitness: ScriptWitness): Try[Psbt] = {
    if (inputIndex >= inputs.length) {
      Failure(new IllegalArgumentException("input index must exist in the input tx"))
    } else {
      inputs(inputIndex) match {
        case input if input.witnessUtxo.isEmpty => Failure(new IllegalArgumentException("cannot finalize: witness utxo is missing"))
        case input =>
          val scriptSig = input.redeemScript.map(script => OP_PUSHDATA(Script.write(script)) :: Nil)
          val finalizedInput = input.copy(
            sighashType = None,
            partialSigs = Map.empty,
            derivationPaths = Map.empty,
            redeemScript = None,
            witnessScript = None,
            scriptSig = scriptSig,
            scriptWitness = Some(scriptWitness)
          )
          Success(this.copy(inputs = this.inputs.updated(inputIndex, finalizedInput)))
      }
    }
  }

  /**
   * Implements the PSBT extractor role: extracts a valid transaction from the psbt data.
   *
   * @return a fully signed, ready-to-broadcast transaction.
   */
  def extract(): Try[Transaction] = {
    val (finalTxsIn, utxos) = global.tx.txIn.zip(inputs).map {
      case (_, input) if !isFinal(input) => return Failure(new IllegalArgumentException("cannot extract transaction: some inputs are not finalized"))
      case (txIn, input) =>
        val finalTxIn = txIn.copy(
          witness = input.scriptWitness.getOrElse(ScriptWitness.empty),
          signatureScript = input.scriptSig.map(Script.write).getOrElse(ByteVector.empty))
        val utxo = input match {
          case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) if utxo.txid != txIn.outPoint.txid => return Failure(new IllegalArgumentException("cannot extract transaction: non-witness utxo does not match unsigned tx input"))
          case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) if utxo.txOut.length <= txIn.outPoint.index => return Failure(new IllegalArgumentException("cannot extract transaction: non-witness utxo index out of bounds"))
          case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) => utxo.txOut(txIn.outPoint.index.toInt)
          case PartiallySignedInput(_, Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _) => utxo
          case _ => return Failure(new IllegalArgumentException("cannot extract transaction: some utxos are missing"))
        }
        (finalTxIn, txIn.outPoint -> utxo)
    }.unzip
    val finalTx = global.tx.copy(txIn = finalTxsIn)
    Try {
      Transaction.correctlySpends(finalTx, utxos.toMap, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
    }.map(_ => finalTx)
  }

  /**
   * Compute the fees paid by the PSBT.
   * Note that if some inputs have not been updated yet, the fee cannot be computed.
   */
  def computeFees(): Try[Satoshi] = {
    val amountOut = global.tx.txOut match {
      case Nil => 0 sat
      case txOut => txOut.map(_.amount).sum
    }
    val amountIn = inputs.zip(global.tx.txIn).foldLeft(Success(0 sat): Try[Satoshi]) {
      case (Failure(ex), _) => Failure(ex)
      case (Success(amount), (input, txIn)) =>
        val inputAmount_opt = input.witnessUtxo.map(_.amount).orElse(input.nonWitnessUtxo.map(tx => tx.txOut(txIn.outPoint.index.toInt).amount))
        inputAmount_opt match {
          case Some(inputAmount) => Success(amount + inputAmount)
          case None => Failure(new IllegalArgumentException(s"input ${txIn.outPoint} has not been updated: amount unknown"))
        }
    }
    amountIn.map(_ - amountOut)
  }

}

/**
 * Partially signed bitcoin transactions: see https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki
 */
object Psbt {

  /** Only version 0 is supported for now. */
  val Version: Long = 0

  /**
   * @param prefix               extended public key version bytes.
   * @param masterKeyFingerprint fingerprint of the master key.
   * @param extendedPublicKey    BIP32 extended public key.
   */
  case class ExtendedPublicKeyWithMaster(prefix: Long, masterKeyFingerprint: Long, extendedPublicKey: ExtendedPublicKey)

  /**
   * @param masterKeyFingerprint fingerprint of the master key.
   * @param keyPath              bip 32 derivation path.
   */
  case class KeyPathWithMaster(masterKeyFingerprint: Long, keyPath: KeyPath)

  case class DataEntry(key: ByteVector, value: ByteVector)

  /** A PSBT is a collection of key-value maps. */
  sealed trait DataMap {
    /** Unknown key-value pairs should be ignored but included in the PSBT. */
    def unknown: Seq[DataEntry]
  }

  /**
   * Global data for the PSBT.
   *
   * @param version            psbt version.
   * @param tx                 partially signed transaction.
   * @param extendedPublicKeys (optional) extended public keys used when signing inputs and producing outputs.
   * @param unknown            (optional) unknown global entries.
   */
  case class Global(version: Long, tx: Transaction, extendedPublicKeys: Seq[ExtendedPublicKeyWithMaster], unknown: Seq[DataEntry]) extends DataMap

  /**
   * A partially signed input. A valid PSBT must contain one such input per input of the [[Global.tx]].
   *
   * @param nonWitnessUtxo  non-witness utxo, used when spending non-segwit outputs.
   * @param witnessUtxo     witness utxo, used when spending segwit outputs.
   * @param sighashType     sighash type to be used when producing signature for this output.
   * @param partialSigs     signatures as would be pushed to the stack from a scriptSig or witness.
   * @param derivationPaths derivation paths used for the signatures.
   * @param redeemScript    redeemScript for this input if it has one.
   * @param witnessScript   witnessScript for this input if it has one.
   * @param scriptSig       fully constructed scriptSig with signatures and any other scripts necessary for the input to pass validation.
   * @param scriptWitness   fully constructed scriptWitness with signatures and any other scripts necessary for the input to pass validation.
   * @param unknown         (optional) unknown global entries.
   */
  case class PartiallySignedInput(nonWitnessUtxo: Option[Transaction],
                                  witnessUtxo: Option[TxOut],
                                  sighashType: Option[Int],
                                  partialSigs: Map[PublicKey, ByteVector],
                                  derivationPaths: Map[PublicKey, KeyPathWithMaster],
                                  redeemScript: Option[Seq[ScriptElt]],
                                  witnessScript: Option[Seq[ScriptElt]],
                                  scriptSig: Option[Seq[ScriptElt]],
                                  scriptWitness: Option[ScriptWitness],
                                  ripemd160: Set[ByteVector],
                                  sha256: Set[ByteVector],
                                  hash160: Set[ByteVector],
                                  hash256: Set[ByteVector],
                                  unknown: Seq[DataEntry]) extends DataMap

  object PartiallySignedInput {
    val empty: PartiallySignedInput = PartiallySignedInput(None, None, None, Map.empty, Map.empty, None, None, None, None, Set.empty, Set.empty, Set.empty, Set.empty, Nil)
  }

  /**
   * A partially signed output. A valid PSBT must contain one such output per output of the [[Global.tx]].
   *
   * @param redeemScript    redeemScript for this output if it has one.
   * @param witnessScript   witnessScript for this output if it has one.
   * @param derivationPaths derivation paths used to produce the public keys associated to this output.
   * @param unknown         (optional) unknown global entries.
   */
  case class PartiallySignedOutput(redeemScript: Option[Seq[ScriptElt]],
                                   witnessScript: Option[Seq[ScriptElt]],
                                   derivationPaths: Map[PublicKey, KeyPathWithMaster],
                                   unknown: Seq[DataEntry]) extends DataMap

  object PartiallySignedOutput {
    val empty: PartiallySignedOutput = PartiallySignedOutput(None, None, Map.empty, Nil)
  }

  /**
   * Implements the PSBT creator role; initializes a PSBT for the given unsigned transaction.
   *
   * @param tx unsigned transaction skeleton.
   * @return the psbt with empty inputs and outputs.
   */
  def apply(tx: Transaction): Psbt = Psbt(
    Global(Version, tx.copy(txIn = tx.txIn.map(_.copy(signatureScript = ByteVector.empty, witness = ScriptWitness.empty))), Nil, Nil),
    tx.txIn.map(_ => PartiallySignedInput.empty),
    tx.txOut.map(_ => PartiallySignedOutput.empty)
  )

  private def sign(priv: PrivateKey, inputIndex: Int, input: PartiallySignedInput, global: Global): Try[PartiallySignedInput] = {
    val txIn = global.tx.txIn(inputIndex)
    input match {
      case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) if utxo.txid != txIn.outPoint.txid => Failure(new IllegalArgumentException("non-witness utxo does not match unsigned tx input"))
      case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) if utxo.txOut.length <= txIn.outPoint.index => Failure(new IllegalArgumentException("non-witness utxo index out of bounds"))
      case PartiallySignedInput(_, Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _) if !Script.isNativeWitnessScript(utxo.publicKeyScript) && !Script.isPayToScript(utxo.publicKeyScript) => Failure(new IllegalArgumentException("witness utxo must use native witness program or P2SH witness program"))
      case PartiallySignedInput(_, Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _) => signWitness(priv, inputIndex, input, global, utxo)
      case PartiallySignedInput(Some(utxo), _, _, _, _, _, _, _, _, _, _, _, _, _) => signNonWitness(priv, inputIndex, input, global, utxo)
      case input => Success(input)
    }
  }

  private def signNonWitness(priv: PrivateKey, inputIndex: Int, input: PartiallySignedInput, global: Global, utxo: Transaction): Try[PartiallySignedInput] = {
    val txIn = global.tx.txIn(inputIndex)
    val redeemScript: Try[Seq[ScriptElt]] = input.redeemScript match {
      case Some(script) =>
        // If a redeem script is provided in the partially signed input, the utxo must be a p2sh for that script.
        val p2sh = Script.write(Script.pay2sh(script))
        if (utxo.txOut(txIn.outPoint.index.toInt).publicKeyScript != p2sh) {
          Failure(new IllegalArgumentException("redeem script does not match non-witness utxo scriptPubKey"))
        } else {
          Success(script)
        }
      case None => Success(Script.parse(utxo.txOut(txIn.outPoint.index.toInt).publicKeyScript))
    }
    redeemScript.map(script => {
      val amount = utxo.txOut(txIn.outPoint.index.toInt).amount
      val sig = Transaction.signInput(global.tx, inputIndex, script, input.sighashType.getOrElse(SIGHASH_ALL), amount, SigVersion.SIGVERSION_BASE, priv)
      input.copy(partialSigs = input.partialSigs + (priv.publicKey -> sig))
    })
  }

  private def signWitness(priv: PrivateKey, inputIndex: Int, input: PartiallySignedInput, global: Global, utxo: TxOut): Try[PartiallySignedInput] = {
    val redeemScript: Try[Seq[ScriptElt]] = input.redeemScript match {
      case Some(script) =>
        // If a redeem script is provided in the partially signed input, the utxo must be a p2sh for that script.
        val p2sh = Script.write(Script.pay2sh(script))
        if (utxo.publicKeyScript != p2sh) {
          Failure(new IllegalArgumentException("redeem script does not match witness utxo scriptPubKey"))
        } else {
          Success(script)
        }
      case None => Success(Script.parse(utxo.publicKeyScript))
    }
    redeemScript.flatMap(script => input.witnessScript match {
      case Some(witnessScript) => if (!Script.isPay2wpkh(script) && script != Script.pay2wsh(witnessScript)) {
        Failure(new IllegalArgumentException("witness script does not match redeemScript or scriptPubKey"))
      } else {
        val sig = Transaction.signInput(global.tx, inputIndex, witnessScript, input.sighashType.getOrElse(SIGHASH_ALL), utxo.amount, SigVersion.SIGVERSION_WITNESS_V0, priv)
        Success(input.copy(partialSigs = input.partialSigs + (priv.publicKey -> sig)))
      }
      case None =>
        val sig = Transaction.signInput(global.tx, inputIndex, script, input.sighashType.getOrElse(SIGHASH_ALL), utxo.amount, SigVersion.SIGVERSION_WITNESS_V0, priv)
        Success(input.copy(partialSigs = input.partialSigs + (priv.publicKey -> sig)))
    })
  }

  /**
   * Implements the PSBT combiner role: combines multiple psbts for the same unsigned transaction.
   *
   * @param psbts partially signed bitcoin transactions to combine.
   * @return a psbt that contains data from all the input psbts.
   */
  def combine(psbts: Psbt*): Try[Psbt] = {
    if (psbts.map(_.global.tx.txid).toSet.size != 1) {
      Failure(new IllegalArgumentException("cannot combine psbts for distinct transactions"))
    } else {
      val global = psbts.head.global.copy(
        unknown = combineUnknown(psbts.map(_.global.unknown)),
        extendedPublicKeys = combineExtendedPublicKeys(psbts.map(_.global.extendedPublicKeys)))
      Success(Psbt(
        global,
        global.tx.txIn.indices.map(i => combineInput(psbts.map(_.inputs(i)))),
        global.tx.txOut.indices.map(i => combineOutput(psbts.map(_.outputs(i))))
      ))
    }
  }

  private def combineUnknown(unknowns: Seq[Seq[DataEntry]]): Seq[DataEntry] =
    unknowns.flatten.map(unknown => unknown.key -> unknown).toMap.values.toSeq

  private def combineExtendedPublicKeys(keys: Seq[Seq[ExtendedPublicKeyWithMaster]]): Seq[ExtendedPublicKeyWithMaster] =
    keys.flatten.map(key => key.extendedPublicKey -> key).toMap.values.toSeq

  private def combineInput(inputs: Seq[PartiallySignedInput]): PartiallySignedInput = PartiallySignedInput(
    inputs.flatMap(_.nonWitnessUtxo).headOption,
    inputs.flatMap(_.witnessUtxo).headOption,
    inputs.flatMap(_.sighashType).headOption,
    inputs.flatMap(_.partialSigs).toMap,
    inputs.flatMap(_.derivationPaths).toMap,
    inputs.flatMap(_.redeemScript).headOption,
    inputs.flatMap(_.witnessScript).headOption,
    inputs.flatMap(_.scriptSig).headOption,
    inputs.flatMap(_.scriptWitness).headOption,
    inputs.flatMap(_.ripemd160).toSet,
    inputs.flatMap(_.sha256).toSet,
    inputs.flatMap(_.hash160).toSet,
    inputs.flatMap(_.hash256).toSet,
    combineUnknown(inputs.map(_.unknown))
  )

  private def combineOutput(outputs: Seq[PartiallySignedOutput]): PartiallySignedOutput = PartiallySignedOutput(
    outputs.flatMap(_.redeemScript).headOption,
    outputs.flatMap(_.witnessScript).headOption,
    outputs.flatMap(_.derivationPaths).toMap,
    combineUnknown(outputs.map(_.unknown))
  )

  /**
   * Joins multiple distinct PSBTs with different inputs and outputs into one PSBT with inputs and outputs from all of
   * the PSBTs. No input in any of the PSBTs can be in more than one of the PSBTs.
   *
   * @param psbts partially signed bitcoin transactions to join.
   * @return a psbt that contains data from all the input psbts.
   */
  def join(psbts: Psbt*): Try[Psbt] = {
    if (psbts.isEmpty) {
      Failure(new IllegalArgumentException("cannot join psbts: no psbt provided"))
    } else if (psbts.map(_.global.version).toSet.size != 1) {
      Failure(new IllegalArgumentException("cannot join psbts with different versions"))
    } else if (psbts.map(_.global.tx.version).toSet.size != 1) {
      Failure(new IllegalArgumentException("cannot join psbts with different tx versions"))
    } else if (psbts.map(_.global.tx.lockTime).toSet.size != 1) {
      Failure(new IllegalArgumentException("cannot join psbts with different tx lockTime"))
    } else if (psbts.flatMap(_.global.tx.txIn.map(_.outPoint)).toSet.size != psbts.map(_.global.tx.txIn.size).sum) {
      Failure(new IllegalArgumentException("cannot join psbts that spend the same input"))
    } else {
      val global = psbts.head.global.copy(
        tx = psbts.head.global.tx.copy(
          txIn = psbts.flatMap(_.global.tx.txIn),
          txOut = psbts.flatMap(_.global.tx.txOut)
        ),
        extendedPublicKeys = psbts.flatMap(_.global.extendedPublicKeys).distinct,
        unknown = psbts.flatMap(_.global.unknown).distinct
      )
      Success(psbts.head.copy(
        global = global,
        inputs = psbts.flatMap(_.inputs),
        outputs = psbts.flatMap(_.outputs)
      ))
    }
  }

  private def isFinal(in: PartiallySignedInput): Boolean = {
    // Everything except the utxo, the scriptSigs and unknown keys must be empty.
    val emptied = in.redeemScript.isEmpty && in.witnessScript.isEmpty && in.partialSigs.isEmpty && in.derivationPaths.isEmpty && in.sighashType.isEmpty
    // And we must have complete scriptSig for either a witness or non-witness utxo.
    val hasWitnessData = in.witnessUtxo.nonEmpty && in.scriptWitness.nonEmpty
    val hasNonWitnessData = in.nonWitnessUtxo.nonEmpty && in.scriptSig.nonEmpty
    emptied && (hasWitnessData || hasNonWitnessData)
  }

  // @formatter:off
  def read(input: InputStream): Try[Psbt] = Codecs.read(input)
  def read(input: Array[Byte]): Try[Psbt] = read(new ByteArrayInputStream(input))
  def read(input: ByteVector): Try[Psbt] = read(input.toArray)
  def fromBase64(input: String): Try[Psbt] = ByteVector.fromBase64(input) match {
    case Some(b) => read(b)
    case None => Failure(new IllegalArgumentException("psbt is not correctly base64-encoded"))
  }
  def write(psbt: Psbt, output: OutputStream): Unit = Codecs.write(psbt, output)
  def write(psbt: Psbt): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    write(psbt, output)
    output.toByteArray
  }
  def toBase64(psbt: Psbt): String = ByteVector(write(psbt)).toBase64
  // @formatter:on

  object Codecs {

    def read(input: InputStream): Try[Psbt] = for {
      _ <- readMagicBytes(input)
      _ <- readSeparator(input)
      global <- readGlobal(input)
      inputs <- readInputs(input, global.tx.txIn)
      outputs <- readOutputs(input, global.tx.txOut.length)
    } yield Psbt(global, inputs, outputs)

    def readNBytes(input: InputStream, n: Int): Array[Byte] = {
      val target = new Array[Byte](n)
      input.read(target)
      target
    }

    private def readMagicBytes(input: InputStream): Try[Boolean] = Try {
      readNBytes(input, 4).toList
    } match {
      case Success(0x70 :: 0x73 :: 0x62 :: 0x74 :: Nil) => Success(true)
      case _ => Failure(new IllegalArgumentException("invalid magic bytes: psbt must start with 0x70736274"))
    }

    private def readSeparator(input: InputStream): Try[Boolean] = Try {
      input.read()
    } match {
      case Success(0xff) => Success(true)
      case _ => Failure(new IllegalArgumentException("magic bytes must be followed by the 0xff separator"))
    }

    private def readGlobal(input: InputStream): Try[Global] = readDataMap(input).flatMap(entries => {
      val keyTypes = Set(0x00, 0x01, 0xfb).map(_.toByte)
      val (known, unknown) = entries.partition(entry => entry.key.headOption.exists(keyTypes.contains))
      val version_opt: Try[Long] = known.find(_.key.head == 0xfb).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt version key must contain exactly 1 byte"))
        case DataEntry(_, value) if value.length != 4 => Failure(new IllegalArgumentException("psbt version must be exactly 4 bytes"))
        case DataEntry(_, value) => Protocol.uint32(value, ByteOrder.LITTLE_ENDIAN) match {
          case v if v > Version => Failure(new IllegalArgumentException(s"unsupported psbt version: $v")): Try[Long]
          case v => Success(v)
        }
      }.getOrElse(Success(0))
      val tx_opt: Try[Transaction] = known.find(_.key.head == 0x00).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt tx key must contain exactly 1 byte"))
        case DataEntry(_, value) =>
          val tx = Transaction.read(value.toArray, Protocol.PROTOCOL_VERSION | Transaction.SERIALIZE_TRANSACTION_NO_WITNESS)
          if (tx.txIn.exists(input => input.hasWitness || input.signatureScript.nonEmpty)) {
            Failure(new IllegalArgumentException("psbt tx inputs must have empty scriptSigs and witness")): Try[Transaction]
          } else {
            Success(tx)
          }
      }.getOrElse(Failure(new IllegalArgumentException("psbt must contain a transaction")))
      val xpubs_opt: Try[Seq[ExtendedPublicKeyWithMaster]] = trySequence(known.filter(_.key.head == 0x01).map {
        case DataEntry(key, _) if key.tail.length != 78 => Failure(new IllegalArgumentException("psbt bip32 xpub must contain exactly 78 bytes"))
        case DataEntry(key, value) =>
          val xpub = new ByteArrayInputStream(key.toArray.tail)
          val prefix = Protocol.uint32(xpub, ByteOrder.BIG_ENDIAN)
          val depth = Protocol.uint8(xpub)
          val parent = Protocol.uint32(xpub, ByteOrder.BIG_ENDIAN)
          val childNumber = Protocol.uint32(xpub, ByteOrder.BIG_ENDIAN)
          val chaincode = ByteVector32(Protocol.bytes(xpub, 32))
          val publicKey = Protocol.bytes(xpub, 33)
          if (value.length != 4 * (depth + 1)) {
            Failure(new IllegalArgumentException("psbt bip32 xpub must contain the master key fingerprint and derivation path"))
          } else {
            val masterKeyFingerprint = Protocol.uint32(value.take(4), ByteOrder.BIG_ENDIAN)
            val derivationPath = KeyPath((0 until depth).map(i => Protocol.uint32(value.slice(4 * (i + 1), 4 * (i + 2)), ByteOrder.LITTLE_ENDIAN)))
            if (derivationPath.lastChildNumber != childNumber) {
              Failure(new IllegalArgumentException("psbt xpub last child number mismatch")): Try[ExtendedPublicKeyWithMaster]
            } else {
              Success(ExtendedPublicKeyWithMaster(prefix, masterKeyFingerprint, ExtendedPublicKey(publicKey, chaincode, depth, derivationPath, parent)))
            }
          }
      })
      for {
        version <- version_opt
        tx <- tx_opt
        xpubs <- xpubs_opt
      } yield Global(version, tx, xpubs, unknown)
    })

    private def readInputs(input: InputStream, txsIn: Seq[TxIn]): Try[Seq[PartiallySignedInput]] = trySequence(txsIn.map(txIn => readInput(input, txIn)))

    private def readInput(input: InputStream, txIn: TxIn): Try[PartiallySignedInput] = readDataMap(input).flatMap(entries => {
      val keyTypes = Set(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x0a, 0x0b, 0x0c, 0x0d).map(_.toByte)
      val (known, unknown) = entries.partition(entry => entry.key.headOption.exists(keyTypes.contains))
      val nonWitnessUtxo_opt: Try[Option[Transaction]] = known.find(_.key.head == 0x00).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt non-witness utxo key must contain exactly 1 byte"))
        case DataEntry(_, value) =>
          val inputTx = Transaction.read(value.toArray)
          if (inputTx.txid == txIn.outPoint.txid && txIn.outPoint.index < inputTx.txOut.length) {
            Success(Some(inputTx))
          } else {
            Failure(new IllegalArgumentException("psbt non-witness utxo does not match psbt outpoint")): Try[Option[Transaction]]
          }
      }.getOrElse(Success(None))
      val witnessUtxo_opt: Try[Option[TxOut]] = known.find(_.key.head == 0x01).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt witness utxo key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(TxOut.read(value.toArray)))
      }.getOrElse(Success(None))
      val partialSigs_opt: Try[Map[PublicKey, ByteVector]] = trySequence(known.filter(_.key.head == 0x02).map {
        case DataEntry(key, value) => Success(PublicKey(key.tail, checkValid = true), value)
      }).map(_.toMap)
      val sighashType_opt: Try[Option[Int]] = known.find(_.key.head == 0x03).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt sighash type key must contain exactly 1 byte"))
        case DataEntry(_, value) if value.length != 4 => Failure(new IllegalArgumentException("psbt sighash type must contain exactly 4 bytes"))
        case DataEntry(_, value) => Success(Some(Protocol.uint32(value, ByteOrder.LITTLE_ENDIAN).toInt))
      }.getOrElse(Success(None))
      val redeemScript_opt: Try[Option[Seq[ScriptElt]]] = known.find(_.key.head == 0x04).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt redeem script key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(Script.parse(value)))
      }.getOrElse(Success(None))
      val witnessScript_opt: Try[Option[Seq[ScriptElt]]] = known.find(_.key.head == 0x05).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt witness script key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(Script.parse(value)))
      }.getOrElse(Success(None))
      val derivationPaths_opt: Try[Map[PublicKey, KeyPathWithMaster]] = trySequence(known.filter(_.key.head == 0x06).map {
        case DataEntry(_, value) if value.length < 4 || value.length % 4 != 0 => Failure(new IllegalArgumentException("psbt bip32 derivation must contain master key fingerprint and child indexes"))
        case DataEntry(key, value) =>
          val publicKey = PublicKey(key.tail, checkValid = true)
          val masterKeyFingerprint = Protocol.uint32(value.take(4), ByteOrder.BIG_ENDIAN)
          val childCount = (value.length.toInt / 4) - 1
          val derivationPath = KeyPath((0 until childCount).map(i => Protocol.uint32(value.slice(4 * (i + 1), 4 * (i + 2)), ByteOrder.LITTLE_ENDIAN)))
          Success(publicKey, KeyPathWithMaster(masterKeyFingerprint, derivationPath))
      }).map(_.toMap)
      val scriptSig_opt: Try[Option[Seq[ScriptElt]]] = known.find(_.key.head == 0x07).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt script sig key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(Script.parse(value)))
      }.getOrElse(Success(None))
      val scriptWitness_opt: Try[Option[ScriptWitness]] = known.find(_.key.head == 0x08).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt script witness key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(ScriptWitness.read(value.toArray)))
      }.getOrElse(Success(None))
      val ripemd160Preimages = trySequence(known.filter(_.key.head == 0x0a).map {
        case DataEntry(key, _) if key.tail.length != 20 => Failure(new IllegalArgumentException("ripemd160 hash must contain exactly 20 bytes"))
        case DataEntry(key, value) if key.tail != Crypto.ripemd160(value) => Failure(new IllegalArgumentException("invalid ripemd160 preimage"))
        case DataEntry(_, value) => Success(value)
      }).map(_.toSet)
      val sha256Preimages = trySequence(known.filter(_.key.head == 0x0b).map {
        case DataEntry(key, _) if key.tail.length != 32 => Failure(new IllegalArgumentException("sha256 hash must contain exactly 32 bytes"))
        case DataEntry(key, value) if key.tail != Crypto.sha256(value).bytes => Failure(new IllegalArgumentException("invalid sha256 preimage"))
        case DataEntry(_, value) => Success(value)
      }).map(_.toSet)
      val hash160Preimages = trySequence(known.filter(_.key.head == 0x0c).map {
        case DataEntry(key, _) if key.tail.length != 20 => Failure(new IllegalArgumentException("hash160 hash must contain exactly 20 bytes"))
        case DataEntry(key, value) if key.tail != Crypto.hash160(value) => Failure(new IllegalArgumentException("invalid hash160 preimage"))
        case DataEntry(_, value) => Success(value)
      }).map(_.toSet)
      val hash256Preimages = trySequence(known.filter(_.key.head == 0x0d).map {
        case DataEntry(key, _) if key.tail.length != 32 => Failure(new IllegalArgumentException("hash256 hash must contain exactly 32 bytes"))
        case DataEntry(key, value) if key.tail != Crypto.hash256(value).bytes => Failure(new IllegalArgumentException("invalid hash256 preimage"))
        case DataEntry(_, value) => Success(value)
      }).map(_.toSet)
      for {
        nonWitnessUtxo <- nonWitnessUtxo_opt
        witnessUtxo <- witnessUtxo_opt
        sighashType <- sighashType_opt
        partialSigs <- partialSigs_opt
        derivationPaths <- derivationPaths_opt
        redeemScript <- redeemScript_opt
        witnessScript <- witnessScript_opt
        scriptSig <- scriptSig_opt
        scriptWitness <- scriptWitness_opt
        ripemd160 <- ripemd160Preimages
        sha256 <- sha256Preimages
        hash160 <- hash160Preimages
        hash256 <- hash256Preimages
      } yield PartiallySignedInput(nonWitnessUtxo, witnessUtxo, sighashType, partialSigs, derivationPaths, redeemScript, witnessScript, scriptSig, scriptWitness, ripemd160, sha256, hash160, hash256, unknown)
    })

    private def readOutputs(input: InputStream, expectedCount: Int): Try[Seq[PartiallySignedOutput]] = trySequence((0 until expectedCount).map(_ => readOutput(input)))

    private def readOutput(input: InputStream): Try[PartiallySignedOutput] = readDataMap(input).flatMap(entries => {
      val keyTypes = Set(0x00, 0x01, 0x02).map(_.toByte)
      val (known, unknown) = entries.partition(entry => entry.key.headOption.exists(keyTypes.contains))
      val redeemScript_opt: Try[Option[Seq[ScriptElt]]] = known.find(_.key.head == 0x00).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt redeem script key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(Script.parse(value)))
      }.getOrElse(Success(None))
      val witnessScript_opt: Try[Option[Seq[ScriptElt]]] = known.find(_.key.head == 0x01).map {
        case DataEntry(key, _) if key.length != 1 => Failure(new IllegalArgumentException("psbt witness script key must contain exactly 1 byte"))
        case DataEntry(_, value) => Success(Some(Script.parse(value)))
      }.getOrElse(Success(None))
      val derivationPaths_opt: Try[Map[PublicKey, KeyPathWithMaster]] = trySequence(known.filter(_.key.head == 0x02).map {
        case DataEntry(_, value) if value.length < 4 || value.length % 4 != 0 => Failure(new IllegalArgumentException("psbt bip32 derivation must contain master key fingerprint and child indexes"))
        case DataEntry(key, value) =>
          val publicKey = PublicKey(key.tail, checkValid = true)
          val masterKeyFingerprint = Protocol.uint32(value.take(4), ByteOrder.BIG_ENDIAN)
          val childCount = (value.length.toInt / 4) - 1
          val derivationPath = KeyPath((0 until childCount).map(i => Protocol.uint32(value.slice(4 * (i + 1), 4 * (i + 2)), ByteOrder.LITTLE_ENDIAN)))
          Success(publicKey, KeyPathWithMaster(masterKeyFingerprint, derivationPath))
      }).map(_.toMap)
      for {
        redeemScript <- redeemScript_opt
        witnessScript <- witnessScript_opt
        derivationPaths <- derivationPaths_opt
      } yield PartiallySignedOutput(redeemScript, witnessScript, derivationPaths, unknown)
    })

    @tailrec
    private def readDataMap(input: InputStream, entries: Seq[DataEntry] = Nil): Try[Seq[DataEntry]] = readDataEntry(input) match {
      case Success(Some(entry)) => readDataMap(input, entry +: entries)
      case Success(None) => if (entries.map(_.key).toSet.size != entries.size) {
        Failure(new IllegalArgumentException("psbt must not contain duplicate keys"))
      } else {
        Success(entries)
      }
      case Failure(ex) => Failure(ex)
    }

    private def readDataEntry(input: InputStream): Try[Option[DataEntry]] = Try {
      Protocol.varint(input) match {
        case 0 =>
          // 0x00 is used as separator to mark the end of a data map.
          None
        case keyLength =>
          val key = readNBytes(input, keyLength.toInt)
          val value = readNBytes(input, Protocol.varint(input).toInt)
          Some(DataEntry(ByteVector(key), ByteVector(value)))
      }
    }

    private def trySequence[T](elems: Seq[Try[T]]): Try[Seq[T]] = elems.foldLeft(Success(Seq.empty): Try[Seq[T]]) {
      case (Failure(ex), _) => Failure(ex)
      case (Success(_), Failure(ex)) => Failure(ex)
      case (Success(prev), Success(cur)) => Success(prev :+ cur)
    }

    def write(psbt: Psbt, output: OutputStream): Unit = {
      Protocol.writeBytes(Array[Byte](0x70, 0x73, 0x62, 0x74), output) // magic bytes
      Protocol.writeUInt8(0xff, output) // separator
      writeGlobal(psbt.global, output)
      writeInputs(psbt.inputs, output)
      writeOutputs(psbt.outputs, output)
    }

    private def writeGlobal(global: Global, output: OutputStream): Unit = {
      writeDataEntry(DataEntry(hex"00", Transaction.write(global.tx, Protocol.PROTOCOL_VERSION | Transaction.SERIALIZE_TRANSACTION_NO_WITNESS)), output)
      global.extendedPublicKeys.foreach(xpub => {
        val key = new ByteArrayOutputStream()
        Protocol.writeUInt8(0x01, key) // key type
        Protocol.writeUInt32(xpub.prefix, key, ByteOrder.BIG_ENDIAN)
        DeterministicWallet.write(xpub.extendedPublicKey, key)
        val value = new ByteArrayOutputStream()
        Protocol.writeUInt32(xpub.masterKeyFingerprint, value, ByteOrder.BIG_ENDIAN)
        xpub.extendedPublicKey.path.foreach(child => Protocol.writeUInt32(child, value, ByteOrder.LITTLE_ENDIAN))
        writeDataEntry(DataEntry(ByteVector(key.toByteArray), ByteVector(value.toByteArray)), output)
      })
      if (global.version > 0) {
        writeDataEntry(DataEntry(hex"fb", Protocol.writeUInt32(global.version, ByteOrder.LITTLE_ENDIAN)), output)
      }
      global.unknown.foreach(entry => writeDataEntry(entry, output))
      Protocol.writeUInt8(0x00, output) // separator
    }

    private def writeInputs(inputs: Seq[Psbt.PartiallySignedInput], output: OutputStream): Unit = inputs.foreach(input => {
      input.nonWitnessUtxo.foreach(tx => writeDataEntry(DataEntry(hex"00", Transaction.write(tx)), output))
      input.witnessUtxo.foreach(txOut => writeDataEntry(DataEntry(hex"01", TxOut.write(txOut)), output))
      sortPublicKeys(input.partialSigs).foreach { case (publicKey, signature) => writeDataEntry(DataEntry(0x02.toByte +: publicKey.value, signature), output) }
      input.sighashType.foreach(sighashType => writeDataEntry(DataEntry(hex"03", Protocol.writeUInt32(sighashType, ByteOrder.LITTLE_ENDIAN)), output))
      input.redeemScript.foreach(redeemScript => writeDataEntry(DataEntry(hex"04", Script.write(redeemScript)), output))
      input.witnessScript.foreach(witnessScript => writeDataEntry(DataEntry(hex"05", Script.write(witnessScript)), output))
      sortPublicKeys(input.derivationPaths).foreach {
        case (publicKey, path) =>
          val key = 0x06.toByte +: publicKey.value
          val value = Protocol.writeUInt32(path.masterKeyFingerprint, ByteOrder.BIG_ENDIAN) ++ ByteVector.concat(path.keyPath.map(childNumber => Protocol.writeUInt32(childNumber, ByteOrder.LITTLE_ENDIAN)))
          writeDataEntry(DataEntry(key, value), output)
      }
      input.scriptSig.foreach(scriptSig => writeDataEntry(DataEntry(hex"07", Script.write(scriptSig)), output))
      input.scriptWitness.foreach(scriptWitness => writeDataEntry(DataEntry(hex"08", ScriptWitness.write(scriptWitness)), output))
      input.ripemd160.foreach(preimage => writeDataEntry(DataEntry(0x0a.toByte +: Crypto.ripemd160(preimage), preimage), output))
      input.sha256.foreach(preimage => writeDataEntry(DataEntry(0x0b.toByte +: Crypto.sha256(preimage), preimage), output))
      input.hash160.foreach(preimage => writeDataEntry(DataEntry(0x0c.toByte +: Crypto.hash160(preimage), preimage), output))
      input.hash256.foreach(preimage => writeDataEntry(DataEntry(0x0d.toByte +: Crypto.hash256(preimage), preimage), output))
      input.unknown.foreach(entry => writeDataEntry(entry, output))
      Protocol.writeUInt8(0x00, output) // separator
    })

    private def writeOutputs(outputs: Seq[Psbt.PartiallySignedOutput], out: OutputStream): Unit = outputs.foreach(output => {
      output.redeemScript.foreach(redeemScript => writeDataEntry(DataEntry(hex"00", Script.write(redeemScript)), out))
      output.witnessScript.foreach(witnessScript => writeDataEntry(DataEntry(hex"01", Script.write(witnessScript)), out))
      sortPublicKeys(output.derivationPaths).foreach {
        case (publicKey, path) =>
          val key = 0x02.toByte +: publicKey.value
          val value = Protocol.writeUInt32(path.masterKeyFingerprint, ByteOrder.BIG_ENDIAN) ++ ByteVector.concat(path.keyPath.map(childNumber => Protocol.writeUInt32(childNumber, ByteOrder.LITTLE_ENDIAN)))
          writeDataEntry(DataEntry(key, value), out)
      }
      output.unknown.foreach(entry => writeDataEntry(entry, out))
      Protocol.writeUInt8(0x00, out) // separator
    })

    private def writeDataEntry(entry: DataEntry, output: OutputStream): Unit = {
      Protocol.writeVarint(entry.key.length, output)
      Protocol.writeBytes(entry.key, output)
      Protocol.writeVarint(entry.value.length, output)
      Protocol.writeBytes(entry.value, output)
    }

    /** We use lexicographic ordering on the public keys. */
    private def sortPublicKeys[T](publicKeys: Map[PublicKey, T]): Seq[(PublicKey, T)] = publicKeys.toSeq.sortWith {
      case ((pk1, _), (pk2, _)) => LexicographicalOrdering.isLessThan(pk1.value, pk2.value)
    }

  }

}
