package com.indicator;

import immortan.Channel;
import immortan.Channel$;
import android.content.Context;
import android.util.DisplayMetrics;
import android.widget.LinearLayout;
import android.util.AttributeSet;
import com.btcontract.wallettest.R;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;


public class ChannelIndicatorLine extends LinearLayout {
    private final static int DEFAULT_INDICATOR_HEIGHT = 8;

    protected int mIndicatorMargin = -1;
    protected int mIndicatorHeight = -1;
    protected int mIndicatorWidth = -1;

    public ChannelIndicatorLine(Context context) {
        super(context);
        initialize();
    }

    public ChannelIndicatorLine(Context context, AttributeSet attrs) {
        super(context, attrs);
        initialize();
    }

    public ChannelIndicatorLine(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initialize();
    }

    public void initialize() {
        DisplayMetrics metrics = getResources().getDisplayMetrics();
        int miniSize = (int) (TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, DEFAULT_INDICATOR_HEIGHT, metrics) + 0.5f);

        mIndicatorHeight = miniSize;
        mIndicatorWidth = miniSize * 3;
        mIndicatorMargin = 10;

        setOrientation(HORIZONTAL);
        setGravity(Gravity.START);
    }

    public void createIndicators(Channel[] channels) {
        // Diff View
        int childViewCount = getChildCount();
        int count = channels.length;

        if (count < childViewCount) {
            removeViews(count, childViewCount - count);
        } else if (count > childViewCount) {
            int addCount = count - childViewCount;
            for (int i = 0; i < addCount; i++) {
                addIndicator();
            }
        }

        // Bind Style
        for (int i = 0; i < count; i++) {
            View indicator = getChildAt(i);
            Channel chan = channels[i];
            setView(indicator, chan);
        }
    }

    public static void setView(View indicator, Channel chan) {
        if (Channel$.MODULE$.isWaiting(chan)) {
            indicator.setBackgroundResource(R.drawable.indicator_chan_confirming);
            indicator.setAlpha(0.6f);
        } else if (!Channel$.MODULE$.isOperational(chan)) {
            indicator.setBackgroundResource(R.drawable.indicator_chan_malfunction);
            indicator.setAlpha(0.6f);
        } else if (Channel$.MODULE$.isOperationalAndSleeping(chan)) {
            indicator.setBackgroundResource(R.drawable.indicator_chan_normal);
            indicator.setAlpha(0.25f);
        } else {
            indicator.setBackgroundResource(R.drawable.indicator_chan_normal);
            indicator.setAlpha(0.8f);
        }
    }

    protected void addIndicator() {
        View indicator = new View(getContext());
        final LayoutParams params = generateDefaultLayoutParams();
        params.width = mIndicatorWidth;
        params.height = mIndicatorHeight;
        params.rightMargin = mIndicatorMargin;
        addView(indicator, params);
    }
}
