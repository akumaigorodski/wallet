package com.btcontract.wallet.lightning

import Tools._
import org.bitcoinj.core.ECKey


abstract class Channel(state: List[Symbol], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>
  val authHandler: AuthHandler

  def doProcess(change: Any) = (change, data, state) match {
    case (paramsWithAnchor: OurChannelParams, null, 'inactive :: rest) if paramsWithAnchor.anchorAmount.isDefined =>
      authHandler process paramsWithAnchor.toOpenProto(proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR)
      become(paramsWithAnchor, 'openWaitForOpenWithAnchor)

    case (paramsNoAnchor: OurChannelParams, null, 'inactive :: rest) if paramsNoAnchor.anchorAmount.isEmpty =>
      authHandler process paramsNoAnchor.toOpenProto(proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR)
      become(paramsNoAnchor, 'openWaitForOpenNoAnchor)

    // INIT

    case (pkt: proto.pkt, params: OurChannelParams, 'openWaitForOpenWithAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WONT_CREATE_ANCHOR =>

      val amount = params.anchorAmount.get
      val theirCommitPubKey = proto2ECKey(pkt.open.commit_key)
      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val ourCommitPubKey = ECKey fromPublicOnly params.commitPrivKey.getPubKey
      val anchorTx = Scripts.makeAnchorTx(ourCommitPubKey, theirCommitPubKey, amount)

      authHandler process new proto.open_anchor(bytes2Sha(anchorTx.getHash.getBytes), 0, amount)
      become(WaitForCommitSig(params, TheirChannelParams(pkt.open.delay.blocks, theirCommitPubKey,
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), anchorTx,
        TheirCommit(0, CommitmentSpec(Set.empty, pkt.open.initial_fee_rate, initAmountUsMsat = 0,
          initAmountThemMsat = amount * 1000, amountUsMsat = 0, amountThemMsat = amount * 1000),
          theirRevocationHash), theirNextRevocationHash), 'openWaitForCommitSig)

    case (pkt: proto.pkt, params: OurChannelParams, 'openWaitForOpenNoAnchor :: rest)
      if pkt.open != null && pkt.open.anch == proto.open_channel.anchor_offer.WILL_CREATE_ANCHOR =>

      val theirRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      val theirNextRevocationHash = Tools sha2Bytes pkt.open.revocation_hash
      become(WaitForAnchor(params, TheirChannelParams(pkt.open.delay.blocks, proto2ECKey(pkt.open.commit_key),
        proto2ECKey(pkt.open.final_key), pkt.open.min_depth, pkt.open.initial_fee_rate), theirRevocationHash,
        theirNextRevocationHash), 'openWaitForAnchor)

    case (pkt: proto.pkt, WaitForCommitSig(ourParams, theirParams, anchorTx, initialCommit,
      theirNextRevocationHash), 'openWaitForCommitSig :: rest) if pkt.open_commit_sig != null =>

    case (pkt: proto.pkt, WaitForAnchor(ourParams, theirParams, theirRevocationHash,
      theirNextRevocationHash), 'openWaitForAnchor :: rest) if pkt.open_anchor != null =>


    case (something: Any, _, _) =>
      // Let know if received an unhandled message in some state
      println(s"Unhandled $something in Channel at $state : $data")
  }
}
