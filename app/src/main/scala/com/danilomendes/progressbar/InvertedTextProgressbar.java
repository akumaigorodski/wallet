package com.danilomendes.progressbar;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.os.SystemClock;
import android.util.AttributeSet;

import androidx.appcompat.widget.AppCompatImageView;

import com.btcontract.wallet.R;

/**
 * ImageView that is animated like a progress view and clips
 * the text. This can be used to animate a progress like view
 * and overlap texts changing the color of the text.
 * The text and the bar are draw on canvas.
 *
 * If the pivot positions are not set, then the center of the
 * canvas will be used.
 *
 * Created by danilo on 24-03-2016.
 */
public class InvertedTextProgressbar extends AppCompatImageView {

    /**
     * Text displayed in the progress bar.
     */
    private String mText = "";

    /**
     * Uninitialized integer value.
     */
    private static final int UNINITIALIZED_INT_VALUE = -1;

    /**
     * Rectangle to draw on canvas.
     */
    private Rect mRect = new Rect();

    /**
     * Flag that indicates if the bar is being animated.
     */
    private boolean mIsAnimating = false;

    private long mStartTime;
    private int mDurationMs;
    private long endTime;

    private int mMaxProgress = -1;
    private int mMinProgress = -1;
    private int mCurrProgress = -1;

    /**
     * X position of the text to be draw.
     */
    private int mPosX = -1;

    /**
     * Y position of the text to be draw.
     */
    private int mPosY = -1;

    /**
     * Paint to use for drawing the text.
     */
    private Paint mTextPaint;

    /**
     * Paint to use for drawing the text.
     */
    private Paint mTextInvertedPaint;

    /**
     * Callback observer.
     */
    private Callback mCallback;

    public InvertedTextProgressbar(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        initComponents(context, attrs, defStyle, 0);
    }

    public InvertedTextProgressbar(Context context, AttributeSet attrs) {
        super(context, attrs);
        initComponents(context, attrs, 0, 0);
    }

    public InvertedTextProgressbar(Context context) {
        super(context);
    }

    public InvertedTextProgressbar(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr);
        initComponents(context, attrs, defStyleAttr, defStyleRes);
    }

    /**
     * Initializes the text paint. This has a fix size.
     *
     * @param attrs The XML attributes to use.
     */
    private void initComponents(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        TypedArray typedArray = context.obtainStyledAttributes(attrs, R.styleable.InvertedTextProgressbar, defStyleAttr, defStyleRes);
        mTextPaint = new Paint();

        // Define the normal text paint.
        mTextPaint.setColor(typedArray.getColor(R.styleable.InvertedTextProgressbar_text_color, Color.BLACK));
        mTextPaint.setStyle(Paint.Style.FILL);
        mTextPaint.setTextSize(typedArray.getDimensionPixelSize(R.styleable.InvertedTextProgressbar_text_size,
                context.getResources().getDimensionPixelSize(R.dimen.text_size_12)));
        mTextPaint.setTypeface(Typeface.defaultFromStyle(typedArray.getInteger(
                R.styleable.InvertedTextProgressbar_text_typeface, Typeface.defaultFromStyle(Typeface.NORMAL).getStyle())));
        mTextPaint.setTextAlign(Paint.Align.CENTER); // Text draw is started in the middle
        mTextPaint.setLinearText(true);
        mTextPaint.setAntiAlias(true);

        // Define the inverted text paint.
        mTextInvertedPaint = new Paint(mTextPaint);
        mTextInvertedPaint.setColor(typedArray.getColor(R.styleable.InvertedTextProgressbar_text_inverted_color, Color.WHITE));

        // Define the text.
        mText = typedArray.getString(R.styleable.InvertedTextProgressbar_text);
        if (mText == null) {
            mText = "Loading...";
        }

        // Set maximum or minimum values if there's any.
        mMaxProgress = typedArray.getInteger(R.styleable.InvertedTextProgressbar_max_progress, UNINITIALIZED_INT_VALUE);
        mMinProgress = typedArray.getInteger(R.styleable.InvertedTextProgressbar_min_progress, UNINITIALIZED_INT_VALUE);

        // Recycle the TypedArray.
        typedArray.recycle();
    }

    @Override
    protected void onDraw(Canvas canvas) {
        canvas.getClipBounds(mRect);

        if (mPosX == -1) {
            mPosX = (getWidth() / 2);
        }

        if (mPosY == -1) {
            mPosY = (int) ((getHeight() / 2) - ((mTextPaint.descent() + mTextPaint.ascent()) / 2));
        }

        // Draw text to overlap.
        if (!mText.isEmpty()) {
            canvas.drawText(mText, mPosX, mPosY, mTextPaint);
        }

        if (mIsAnimating) {
            // Only start timing from first frame of animation
            if (mStartTime == UNINITIALIZED_INT_VALUE) {
                mStartTime = SystemClock.uptimeMillis();
                endTime = mStartTime + mDurationMs;
            }

            // Adjust clip bounds according to the time fraction
            long currentTime = SystemClock.uptimeMillis();
            if (currentTime < endTime) {
                float timeFraction = (currentTime - mStartTime) / (mDurationMs * 1f);
                int alpha = Math.round(mRect.width() * timeFraction);
                mRect.right = mRect.left + alpha;
                canvas.clipRect(mRect);
            } else {
                mIsAnimating = false;
                if (mCallback != null) {
                    mCallback.onAnimationEnd();
                }
            }
        } else if (mMinProgress > -1 && mMaxProgress > mMinProgress &&
                (mCurrProgress >= mMinProgress && mCurrProgress <= mMaxProgress)) {
            mRect.right = mRect.width() * mCurrProgress / mMaxProgress; // Regra de 3 simples.
            canvas.clipRect(mRect);
        }

        // Draw current state.
        super.onDraw(canvas);

        if (!mText.isEmpty()) {
            // Draw text in position set.
            canvas.drawText(mText, mPosX, mPosY, mTextInvertedPaint);
        }

        // Request another draw operation until time is up
        if (mIsAnimating) {
            invalidate();
        }
    }

    /**
     * Sets the text that will overlay.
     *
     * @param text The text to draw.
     */
    public void setText(String text) {
        mText = text;
    }

    /**
     * Gets the current text to draw.
     *
     * @return The current text to draw.
     */
    public String getText() {
        return mText;
    }

    /**
     * Gets the paint currently being used for the overlapped text.
     *
     * @return The overlapped text paint.
     */
    public Paint getTextPaint() {
        return mTextPaint;
    }

    /**
     * Sets the paint to be used for the overlapped text.
     *
     * @param textPaint The Paint to be set for the overlapped text.
     */
    public void setTextPaint(Paint textPaint) {
        this.mTextPaint = mTextPaint;
    }

    /**
     * Gets the paint currently being used for the overlapping text.
     *
     * @return The overlapping text paint.
     */
    public Paint getTextInvertedPaint() {
        return mTextInvertedPaint;
    }

    /**
     * Sets the paint to be used for the overlapping text.
     *
     * @param textInvertedPaint The Paint to be set for the
     * overlapping text.
     */
    public void setTextInvertedPaint(Paint textInvertedPaint) {
        this.mTextInvertedPaint = mTextInvertedPaint;
    }

    /**
     * Sets coordinates of the pivot position of the text to draw.
     * If any of the coordinates are -1 then the text positions
     * will be the center of the canvas.
     *
     * @param x The X position.
     * @param y The Y position.
     */
    public void setTextPivot(int x, int y) {
        this.mPosX = x;
        this.mPosY = y;
    }

    /**
     * Gets the current progress. Note that if it never had been
     * set before this method will return -1.
     *
     * @return The current progress of the progress bar.
     */
    public int getCurrentProgress() {
        return mCurrProgress;
    }

    /**
     * Sets the current progress. Note that it's needed to set
     * maximum and minimum progress values, otherwise nothing
     * will occur.
     *
     * {@link #setMaxProgress(int)}
     * {@link #setMinProgress(int)}
     *
     * @param progress The progress to be set.
     */
    public InvertedTextProgressbar setProgress(int progress) {
        mCurrProgress = progress;
        invalidate();
        return this;
    }

    /**
     * Gets the max progress. Note that if this value has not
     * been set before it will return -1.
     *
     * @return The max progress.
     */
    public int getMaxProgress() {
        return mMaxProgress;
    }

    /**
     * Sets the maximum progress value.
     *
     * @param maxProgress The maximum progress.
     */
    public InvertedTextProgressbar setMaxProgress(int maxProgress) {
        mMaxProgress = maxProgress;
        return this;
    }

    /**
     * Gets the min progress. Note that if this value has not
     * been set before it will return -1.
     *
     * @return The min progress.
     */
    public int getMinProgress() {
        return mMinProgress;
    }

    /**
     * Sets the minimum progress value.
     *
     * @param minProgress The minimum progress.
     */
    public InvertedTextProgressbar setMinProgress(int minProgress) {
        mMinProgress = minProgress;
        return this;
    }

    /**
     * Sets the text size for both paints.
     * If the paints have not been initizialized yet this method
     * won't do anything.
     *
     * @param size The new text size to be set.
     */
    public void setTextSize(int size) {
        if (mTextPaint != null && mTextInvertedPaint != null) {
            mTextPaint.setTextSize(size);
            mTextInvertedPaint.setTextSize(size);
        }
    }

    /**
     * Starts the animation of the progress and text draw.
     *
     * @param durationMs The duration of the bar filling animation.
     */
    public void startAnimation(int durationMs) {
        mIsAnimating = true;
        mStartTime = UNINITIALIZED_INT_VALUE;
        mDurationMs = durationMs;
        invalidate();
    }

    /**
     * Interface that holds methods that are called during the
     * animation.
     */
    public interface Callback {
        void onAnimationEnd();
    }

    /**
     * Sets the callback to handle animation end.
     *
     * @param callback The callback to call upon events.
     */
    public void setCallback(Callback callback) {
        mCallback = callback;
    }
}
