package com.btcontract.wallet

import android.view.GestureDetector.SimpleOnGestureListener
import org.jbox2d.collision.shapes.PolygonShape
import org.bitcoinj.core.TransactionOutput
import android.view.View.OnTouchListener
import android.graphics.PorterDuff.Mode
import android.content.Context
import org.jbox2d.common.Vec2
import android.os.Bundle

import com.btcontract.wallet.Utils.{Outputs, Coins, wrap, randBtw}
import org.jbox2d.dynamics.{BodyDef, FixtureDef, BodyType, World}
import android.animation.{ValueAnimator, Animator}
import android.graphics.{Paint, Color, Canvas}
import android.view._


class WalletActivity extends InfoActivity { me =>
  lazy val sack = findViewById(R.id.sack).asInstanceOf[SurfaceView]
  val coinColors = List(0xFFED9D09, 0xFF3F68FC, 0xFFE0C200, 0xFFF76300,
    0xFFA400FC, 0xFFD6006F, 0xFF008700, 0xFFA10023, 0xFFE31300, 0xFF96B000,
    0xFF6500FC, 0xFF00A8A5, 0xFF627485, 0xFF1BA2E0)

  val initGravity = new Vec2(0.0f, 16.0f)
  val world = new World(initGravity)

  private[this] var coins = List.empty[AbstractCoin]
  private[this] var loop: CanvasLoop = null
  private[this] var paint = new CoinPaint
  private[this] var scaleHeight = 10f
  private[this] var scaleWidth = 5f
  private[this] var scale = 50f

  val walletListener = new WalletListener {
    override def say(txt: String, info: Int) = {
      val bs = -1f :: coins.map(_.body.getPosition.y)
      val out2Coins = coins.groupBy(_.out)

      // Sort out changes
      val fresh = app.kit.freshOuts
      val onScreenOutputs = out2Coins.keys.toList
      val coinsToRemove = onScreenOutputs diff fresh
      val coinsToInsert = fresh diff onScreenOutputs

      // Remove before update
      me runOnUiThread anyToRunnable {
        rmCoins(coinsToRemove flatMap out2Coins)
        addCoins(coinsToInsert, bs.min, 0)
      }

      // Update title
      super.say(txt, info)
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    super.onCreate(savedInstState)
    world setAllowSleep true

    if (app.isAlive) {
      add(constantListener.mkTxt, Informer.PEERS).ui.run
      new Anim(app.kit.currentBalance, Utils.appName)
      setContentView(R.layout.activity_wallet)

      // Wallet, peers and coins callbacks
      app.kit.peerGroup addEventListener new CatchUpTracker
      app.kit.peerGroup addEventListener constantListener
      app.kit.wallet addEventListener walletListener
      sack setOnTouchListener new MoveListener(this)
      sack.getHolder addCallback new OnceCallback
    } else this exitTo classOf[MainActivity]
  }

  // Activity lifecycle listeners management
  override def onOptionsItemSelected(mi: MenuItem) =
  {
    decideActionToTake(mi.getItemId)
    super.onOptionsItemSelected(mi)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.wallet_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onResume = wrap(super.onResume) {
    prefs.edit.putBoolean(AbstractKit.SACK_OR_TXS, true).commit
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.peerGroup removeEventListener constantListener
    app.kit.wallet removeEventListener walletListener
  }

  // Coins management

  def hasRoom = coins.foldLeft(0f)(_ + _.area) < scaleHeight * scaleWidth
  def addCoins(outs: Outputs, base: Float, idx: Int): Unit = if (outs.nonEmpty & hasRoom) {
    def slice(acc: Coins, out: TransactionOutput, lft: Long, mod: Long): Coins = lft % mod match {
      case sum if sum > 0 => slice(app.mk(sum, mod, new Paint(paint.now), out) :: acc, out, lft - sum, mod * 10)
      case sum if lft > 0 => slice(acc, out, lft, mod * 10)
      case sum => acc
    }

    val outCoins = slice(Nil, outs.head, outs.head.getValue.value, 1000)
    world synchronized outCoins.zipWithIndex.foreach { case (coin, ordNum) =>
      app.coinBodyDef setPosition new Vec2(randBtw(coin.coinSize, scaleWidth), base - ordNum - idx)
      coin.body = world createBody app.coinBodyDef
      coin.body createFixture coin.fix
    }

    coins = coins ::: outCoins
    paint = new CoinPaint(paint.num + 1)
    addCoins(outs.tail, base, idx + outCoins.size)
  }

  def rmCoins(cs: Coins) =
    for (Tuple2(coin, ord) <- cs.zipWithIndex) new AnimListener {
      override def onAnimationUpdate(valueAnimator: ValueAnimator) = {
        val currentValue = valueAnimator.getAnimatedValue.asInstanceOf[Int]
        coin.txtPaint setAlpha 200 * currentValue / 100
        coin.bgPaint setAlpha 255 * currentValue / 100
        coin.coinSize -= 0.0025f
      }

      override def onAnimationEnd(an: Animator) = {
        world synchronized world.destroyBody(coin.body)
        coins = coins diff List(coin)
      }

      coin.isGone = true
      setStartDelay(150 * ord)
      start
    }

  // Inner classes

  class MoveListener(ctxt: Context)
  extends SimpleOnGestureListener
  with OnTouchListener
  {
    private[this] val gestDetector = new GestureDetector(ctxt, this)
    private[this] var target = Option.empty[AbstractCoin]
    private[this] var xShift, yShift = 0f

    override def onDown(e: MotionEvent) = true
    override def onFling(motionEvent1: MotionEvent, motionEvent2: MotionEvent, velX: Float, velY: Float) = {
      for (cn <- target) cn.body.applyLinearImpulse(new Vec2(velX / scale, velY / scale), cn.body.getWorldCenter, true)
      super.onFling(motionEvent1, motionEvent2, velX, velY)
    }

    override def onTouch(v: View, event: MotionEvent) = {
      if (event.getAction == MotionEvent.ACTION_MOVE) moveTrg(event.getX, event.getY)
      else if (event.getAction == MotionEvent.ACTION_DOWN) mkTrg(event.getX, event.getY)
      else if (event.getAction == MotionEvent.ACTION_UP) setType(BodyType.DYNAMIC)
      gestDetector onTouchEvent event
    }

    def mkTrg(x: Float, y: Float) = {
      target = coins find { case coin =>
        xShift = x / scale - coin.body.getPosition.x
        yShift = y / scale - coin.body.getPosition.y
        val hypot = coin.coinSize * coin.coinSize
        xShift * xShift + yShift * yShift < hypot
      }

      // Avoid collisions
      setType(BodyType.KINEMATIC)
    }

    def setType(tp: BodyType) = world synchronized {
      for (theCoin <- target) theCoin.body.setType(tp)
    }

    def moveTrg(x: Float, y: Float) = for (theCoin <- target) {
      val moveVector = new Vec2(x / scale - xShift, y / scale - yShift)
      theCoin.body.setTransform(moveVector, 0)
    }
  }

  class CanvasLoop extends Thread {
    var surfceHolder: SurfaceHolder = null
    var canvas: Canvas = null
    var mustRun = true
    start

    def proceed = {
      canvas = surfceHolder.lockCanvas
      canvas.drawColor(Color.TRANSPARENT, Mode.CLEAR)
      world synchronized world.step(1 / 60f, 6, 3)
      for (cn <- coins) cn.draw(canvas, scale)
      Thread sleep 15
    }

    override def run =
      while (mustRun) try proceed
      catch { case _: Throwable => mustRun = false }
      finally if (canvas != null) surfceHolder unlockCanvasAndPost canvas
  }

  class OnceCallback extends HolderCallback {
    override def surfaceCreated(holder: SurfaceHolder) = {
      val (w, h) = (holder.getSurfaceFrame.width, holder.getSurfaceFrame.height)
      scale = scala.math.sqrt(w * w + h * h).toFloat / scrWidth.toFloat / 5.4f
      scaleHeight = h / scale
      scaleWidth = w / scale

      // Walls and coins
      val wallHeight = scaleHeight * 3
      val leftWall = this body new Vec2(-2f, -scaleHeight * 2)
      val rightWall = this body new Vec2(scaleWidth + 2f, -scaleHeight * 2)
      val floor = this body new Vec2(scaleWidth / 2, scaleHeight + 2f)
      val ceil = this body new Vec2(scaleWidth / 2, -scaleHeight * 5)
      world createBody rightWall createFixture shape(2f, wallHeight)
      world createBody leftWall createFixture shape(2f, wallHeight)
      world createBody floor createFixture shape(scaleWidth, 2f)
      world createBody ceil createFixture shape(scaleWidth, 2f)
      addCoins(app.kit.freshOuts, scaleHeight, 0)

      // Avoid repeated walls creation
      val const = new ConstantCallback
      const surfaceCreated holder
      holder removeCallback this
      holder addCallback const
    }

    def shape(width: Float, height2: Float) = {
      val rectangularPolygonShape = new PolygonShape
      rectangularPolygonShape.setAsBox(width, height2)
      new FixtureDef { shape = rectangularPolygonShape }
    }

    def body(pos: Vec2) = {
      val bodyDef = new BodyDef
      bodyDef setType BodyType.STATIC
      bodyDef setPosition pos
      bodyDef
    }
  }

  class ConstantCallback extends HolderCallback {
    override def surfaceCreated(holder: SurfaceHolder) =
      loop = new CanvasLoop { surfceHolder = holder }
  }

  class CoinPaint(val num: Int = 0) {
    val now = new Paint(Paint.ANTI_ALIAS_FLAG)
    now setColor coinColors(num % coinColors.size)
  }
}