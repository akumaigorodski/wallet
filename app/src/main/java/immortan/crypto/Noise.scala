package immortan.crypto

import java.math.BigInteger
import java.nio.ByteOrder.LITTLE_ENDIAN

import fr.acinq.bitcoin.Crypto
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.Protocol.writeUInt64
import fr.acinq.eclair.crypto.{ChaCha20Poly1305, Mac32}
import fr.acinq.eclair.randomBytes
import scodec.bits.ByteVector


object Noise {
  sealed trait MessagePattern
  case object S extends MessagePattern
  case object E extends MessagePattern
  case object EE extends MessagePattern
  case object ES extends MessagePattern
  case object SE extends MessagePattern
  case object SS extends MessagePattern
  type MessagePatterns = List[MessagePattern]
  type MessagePatternsList = List[MessagePatterns]

  case class KeyPair(pub: ByteVector, priv: ByteVector)
  case class HandshakePattern(name: String, initiatorPreMessages: MessagePatterns,
                              responderPreMessages: MessagePatterns, messages: MessagePatternsList)

  val handshakePatternNN: HandshakePattern = HandshakePattern("NN", Nil, Nil, List(E) :: List(E, EE) :: Nil)
  val handshakePatternXK: HandshakePattern = HandshakePattern("XK", Nil, S :: Nil, List(E, ES) :: List(E, EE) :: List(S, SE) :: Nil)


  trait DHFunctions {
    def dh(keyPair: KeyPair, publicKey: ByteVector): ByteVector
    def generateKeyPair(priv: ByteVector): KeyPair

    val dhLen: Int
    val pubKeyLen: Int
    val name: String
  }

  object Secp256k1DHFunctions extends DHFunctions {
    def dh(keyPair: KeyPair, pubKey: ByteVector): ByteVector = {
      val pointGBigInteger = new BigInteger(1, keyPair.priv.take(32).toArray)
      val ecPoint = Crypto.curve.getCurve.decodePoint(pubKey.toArray).multiply(pointGBigInteger)
      Crypto sha256 ByteVector.view(ecPoint.normalize getEncoded true)
    }

    def generateKeyPair(priv: ByteVector): KeyPair = {
      require(priv.length == 32, "DH key must be of length 32")
      KeyPair(PrivateKey(priv).publicKey.value, priv)
    }

    val name = "secp256k1"
    val pubKeyLen = 33
    val dhLen = 32
  }

  trait CipherFunctions {
    def encrypt(k: ByteVector, n: Long, ad: ByteVector, plaintext: ByteVector): ByteVector
    def decrypt(k: ByteVector, n: Long, ad: ByteVector, ciphertext: ByteVector): ByteVector
    val name: String
  }

  object Chacha20Poly1305CipherFunctions extends CipherFunctions {
    def encrypt(k: ByteVector, n: Long, ad: ByteVector, plaintext: ByteVector): ByteVector = {
      val (ciphertext, mac) = ChaCha20Poly1305.encrypt(k, nonce(n), plaintext, ad)
      ciphertext ++ mac
    }

    def decrypt(k: ByteVector, n: Long, ad: ByteVector, ciphertextAndMac: ByteVector): ByteVector = {
      val (ciphertext, mac) = (ciphertextAndMac dropRight 16, ciphertextAndMac takeRight 16)
      ChaCha20Poly1305.decrypt(k, nonce(n), ciphertext, ad, mac)
    }

    def nonce(n: Long): ByteVector =
      baseBin ++ writeUInt64(n, LITTLE_ENDIAN)

    val name = "ChaChaPoly"
    val baseBin: ByteVector = ByteVector.fromValidHex("00000000")
  }

  trait HashFunctions {
    def hmacHash(key: ByteVector, data: ByteVector): ByteVector
    def hash(source: ByteVector): ByteVector

    val blockLen: Int
    val hashLen: Int
    val name: String

    def hkdf(chainingKey: ByteVector, inputMaterial: ByteVector): (ByteVector, ByteVector) = {
      val tempkey = hmacHash(data = inputMaterial, key = chainingKey)
      val firstOutput = hmacHash(data = ByteVector(0x01), key = tempkey)
      val lastOutput = hmacHash(data = firstOutput ++ ByteVector(0x02), key = tempkey)
      firstOutput -> lastOutput
    }
  }

  object SHA256HashFunctions extends HashFunctions {
    def hmacHash(key: ByteVector, data: ByteVector): ByteVector = Mac32.hmac256(key, data).bytes
    def hash(hashingSource: ByteVector): ByteVector = Crypto.sha256(hashingSource)

    val name = "SHA256"
    val blockLen = 64
    val hashLen = 32
  }

  trait CipherState {
    def cipher: CipherFunctions
    def initializeKey(key: ByteVector): CipherState = CipherState(key, cipher)
    def encryptWithAd(ad: ByteVector, plaintext: ByteVector): (CipherState, ByteVector)
    def decryptWithAd(ad: ByteVector, ciphertext: ByteVector): (CipherState, ByteVector)
    val hasKey: Boolean
  }

  case class UnitializedCipherState(cipher: CipherFunctions) extends CipherState { me =>
    def decryptWithAd(ad: ByteVector, ciphertext: ByteVector): (UnitializedCipherState, ByteVector) = me -> ciphertext
    def encryptWithAd(ad: ByteVector, plaintext: ByteVector): (UnitializedCipherState, ByteVector) = me -> plaintext
    val hasKey = false
  }

  case class InitializedCipherState(k: ByteVector, n: Long, cipher: CipherFunctions) extends CipherState { me =>
    def decryptWithAd(ad: ByteVector, ciphertext: ByteVector): (InitializedCipherState, ByteVector) = copy(n = n + 1) -> cipher.decrypt(k, n, ad, ciphertext)
    def encryptWithAd(ad: ByteVector, plaintext: ByteVector): (InitializedCipherState, ByteVector) = copy(n = n + 1) -> cipher.encrypt(k, n, ad, plaintext)
    require(k.length == 32)
    val hasKey = true
  }

  object CipherState {
    def apply(k: ByteVector, cipher: CipherFunctions): CipherState =
      if (k.length == 32) InitializedCipherState(k, 0, cipher)
      else if (k.length == 0) UnitializedCipherState(cipher)
      else throw new Exception("Invalid k length")

    def apply(cipher: CipherFunctions): UnitializedCipherState = UnitializedCipherState(cipher)
  }

  case class SymmetricState(cipherState: CipherState, ck: ByteVector,
                            h: ByteVector, hashFunctions: HashFunctions) {

    def mixKey(inputKeyMaterial: ByteVector): SymmetricState = {
      val (ck1, tempk) = hashFunctions.hkdf(chainingKey = ck, inputMaterial = inputKeyMaterial)
      val tempk1 = hashFunctions.hashLen match { case 32 => tempk case 64 => tempk take 32 }
      copy(cipherState = cipherState initializeKey tempk1, ck = ck1)
    }

    def mixHash(data: ByteVector): SymmetricState =
      copy(h = hashFunctions hash h ++ data)

    def encryptAndHash(plaintext: ByteVector): (SymmetricState, ByteVector) = {
      val (cipherstate1, ciphertext) = cipherState.encryptWithAd(h, plaintext)
      copy(cipherState = cipherstate1).mixHash(ciphertext) -> ciphertext
    }

    def decryptAndHash(ciphertext: ByteVector): (SymmetricState, ByteVector) = {
      val (cipherstate1, plaintext) = cipherState.decryptWithAd(h, ciphertext)
      copy(cipherState = cipherstate1).mixHash(ciphertext) -> plaintext
    }

    def split: (CipherState, CipherState, ByteVector) = {
      val (tempk1, tempk2) = hashFunctions.hkdf(ck, ByteVector.empty)
      val part1 = cipherState.initializeKey(tempk1 take 32)
      val part2 = cipherState.initializeKey(tempk2 take 32)
      (part1, part2, ck)
    }
  }

  object SymmetricState {
    def mkHash(protocolName: ByteVector, hashFunctions: HashFunctions): ByteVector =
      if (protocolName.length > hashFunctions.hashLen) hashFunctions.hash(protocolName)
      else protocolName ++ ByteVector.fill[Byte](hashFunctions.hashLen - protocolName.length)(0)

    def mk(hash: ByteVector, cipherFunctions: CipherFunctions, hashFunctions: HashFunctions) =
      new SymmetricState(CipherState(cipherFunctions), ck = hash, h = hash, hashFunctions)
  }

  sealed trait HandshakeState
  case class HandshakeStateWriter(messages: List[MessagePatterns], state: SymmetricState,
                                  s: KeyPair, e: KeyPair, rs: ByteVector, re: ByteVector,
                                  dh: DHFunctions) extends HandshakeState { me =>

    def toReader: HandshakeStateReader = HandshakeStateReader(messages, state, s, e, rs, re, dh)

    def fold(pts: MessagePatterns): (HandshakeStateWriter, ByteVector) =
      pts.foldLeft(me -> ByteVector.empty) {

        case Tuple2(Tuple2(writer, buffer), E) =>
          val e1 = dh generateKeyPair randomBytes(dh.dhLen)
          (writer.copy(state = writer.state mixHash e1.pub, e = e1), buffer ++ e1.pub)

        case Tuple2(Tuple2(writer, buffer), S) =>
          val (state1, ciphertext) = writer.state encryptAndHash s.pub
          Tuple2(writer.copy(state = state1), buffer ++ ciphertext)

        case Tuple2(Tuple2(writer, buffer), EE) =>
          val state1 = writer.state mixKey dh.dh(writer.e, writer.re)
          Tuple2(writer.copy(state = state1), buffer)

        case Tuple2(Tuple2(writer, buffer), SS) =>
          val state1 = writer.state mixKey dh.dh(writer.s, writer.rs)
          Tuple2(writer.copy(state = state1), buffer)

        case Tuple2(Tuple2(writer, buffer), ES) =>
          val state1 = writer.state mixKey dh.dh(writer.e, writer.rs)
          Tuple2(writer.copy(state = state1), buffer)

        case Tuple2(Tuple2(writer, buffer), SE) =>
          val state1 = writer.state mixKey dh.dh(writer.s, writer.re)
          Tuple2(writer.copy(state = state1), buffer)
      }

    def write(payload: ByteVector): (HandshakeStateReader, (CipherState, CipherState, ByteVector), ByteVector) = {
      val (writer1, buffer1) = fold(messages.head)
      val (state1, ciphertext) = writer1.state encryptAndHash payload
      val writer2 = writer1.copy(messages = messages.tail, state = state1)
      val parts = if (messages.tail.isEmpty) writer2.state.split else null
      (writer2.toReader, parts, buffer1 ++ ciphertext)
    }
  }

  case class HandshakeStateReader(messages: List[MessagePatterns], state: SymmetricState,
                                  s: KeyPair, e: KeyPair, rs: ByteVector, re: ByteVector,
                                  dh: DHFunctions) extends HandshakeState { me =>

    def toWriter: HandshakeStateWriter = HandshakeStateWriter(messages, state, s, e, rs, re, dh)

    def fold(message: ByteVector): (HandshakeStateReader, ByteVector) =
      messages.head.foldLeft(me -> message) {
        case Tuple2(Tuple2(reader, buffer), E) =>
          val (re1, buffer1) = buffer.splitAt(dh.pubKeyLen)
          (reader.copy(state = reader.state mixHash re1, re = re1), buffer1)

        case Tuple2(Tuple2(reader, buffer), S) =>
          val hasKey = reader.state.cipherState.hasKey
          val len = if (hasKey) dh.pubKeyLen + 16 else dh.pubKeyLen

          val (temp, buffer1) = buffer.splitAt(len)
          val (state1, rs1) = reader.state decryptAndHash temp
          Tuple2(reader.copy(state = state1, rs = rs1), buffer1)

        case Tuple2(Tuple2(reader, buffer), EE) =>
          val state1 = reader.state mixKey dh.dh(reader.e, reader.re)
          Tuple2(reader.copy(state = state1), buffer)

        case Tuple2(Tuple2(reader, buffer), SS) =>
          val state1 = reader.state mixKey dh.dh(reader.s, reader.rs)
          Tuple2(reader.copy(state = state1), buffer)

        case Tuple2(Tuple2(reader, buffer), ES) =>
          val state1 = reader.state mixKey dh.dh(reader.s, reader.re)
          Tuple2(reader.copy(state = state1), buffer)

        case Tuple2(Tuple2(reader, buffer), SE) =>
          val state1 = reader.state mixKey dh.dh(reader.e, reader.rs)
          Tuple2(reader.copy(state = state1), buffer)
      }

    def read(message: ByteVector): (HandshakeStateWriter, (CipherState, CipherState, ByteVector), ByteVector) = {
      val (reader1, buffer1) = fold(message)
      val (state1, payload) = reader1.state decryptAndHash buffer1
      val reader2 = reader1.copy(messages = messages.tail, state = state1)
      val parts = if (messages.tail.isEmpty) reader2.state.split else null
      (reader2.toWriter, parts, payload)
    }
  }

  object HandshakeState {
    private def makeSymmetricState(handshakePattern: HandshakePattern, prologue: ByteVector,
                                   dh: DHFunctions, cipher: CipherFunctions, hash: HashFunctions) = {

      val name = "Noise_" + handshakePattern.name + "_" + dh.name + "_" + cipher.name + "_" + hash.name
      val symmetricState = SymmetricState.mk(SymmetricState.mkHash(ByteVector.view(name getBytes "UTF-8"), hash), cipher, hash)
      symmetricState mixHash prologue
    }

    def initializeWriter(handshakePattern: HandshakePattern, prologue: ByteVector,
                         s: KeyPair, e: KeyPair, rs: ByteVector, re: ByteVector, dh: DHFunctions,
                         cipher: CipherFunctions, hash: HashFunctions): HandshakeStateWriter = {

      val symmetricState = makeSymmetricState(handshakePattern, prologue, dh, cipher, hash)
      val symmetricState1 = handshakePattern.initiatorPreMessages.foldLeft(symmetricState) {
        case (_, EE | ES | SE | SS) => throw new Exception("Invalid state initializeWriter")
        case (state, E) => state mixHash e.pub
        case (state, S) => state mixHash s.pub
      }

      val symmetricState2 = handshakePattern.responderPreMessages.foldLeft(symmetricState1) {
        case (_, EE | ES | SE | SS) => throw new Exception("Invalid state initializeWriter")
        case (state, E) => state mixHash re
        case (state, S) => state mixHash rs
      }

      HandshakeStateWriter(handshakePattern.messages, symmetricState2, s, e, rs, re, dh)
    }

    def initializeReader(handshakePattern: HandshakePattern, prologue: ByteVector,
                         s: KeyPair, e: KeyPair, rs: ByteVector, re: ByteVector, dh: DHFunctions,
                         cipher: CipherFunctions, hash: HashFunctions): HandshakeStateReader = {

      val symmetricState = makeSymmetricState(handshakePattern, prologue, dh, cipher, hash)
      val symmetricState1 = handshakePattern.initiatorPreMessages.foldLeft(symmetricState) {
        case (_, EE | ES | SE | SS) => throw new Exception("Invalid state initializeReader")
        case (state, E) => state mixHash re
        case (state, S) => state mixHash rs
      }

      val symmetricState2 = handshakePattern.responderPreMessages.foldLeft(symmetricState1) {
        case (_, EE | ES | SE | SS) => throw new Exception("Invalid state initializeReader")
        case (state, E) => state mixHash e.pub
        case (state, S) => state mixHash s.pub
      }

      HandshakeStateReader(handshakePattern.messages, symmetricState2, s, e, rs, re, dh)
    }
  }
}