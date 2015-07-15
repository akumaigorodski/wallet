package com.btcontract.wallet;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;

import org.bitcoinj.core.TransactionOutput;
import org.jbox2d.collision.shapes.CircleShape;
import org.jbox2d.dynamics.FixtureDef;
import org.jbox2d.dynamics.Body;


public abstract class AbstractCoin
{
    public boolean isGone = false;
    public TransactionOutput out;
    public Paint txtPaint;
    public float coinSize;
    public Paint bgPaint;
    public String text;
    public long value;
    public Body body;

    public static Bitmap bitLogo = null;
    public static float PIFloat = (float) Math.PI;

    public abstract void draw(Canvas canvas, float scale);
    public float area() { return this.isGone ? 0 : AbstractCoin.PIFloat * coinSize * coinSize; }
    public AbstractCoin(Paint bgPaint, float size, String text, long value, Paint txPaint, TransactionOutput out)
    {
        this.txtPaint = txPaint;
        this.bgPaint = bgPaint;
        this.coinSize = size;
        this.value = value;
        this.text = text;
        this.out = out;
    }

    public FixtureDef fix() {
        CircleShape shape = new CircleShape();
        FixtureDef fix = new FixtureDef();
        shape.setRadius(coinSize);
        fix.setRestitution(0.2f);
        fix.setShape(shape);
        return fix;
    }
}