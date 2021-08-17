package com.aurelhubert.ahbottomnavigation;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import androidx.annotation.ColorInt;
import androidx.annotation.ColorRes;
import androidx.annotation.DrawableRes;
import androidx.annotation.StringRes;
import androidx.core.content.ContextCompat;
import androidx.appcompat.content.res.AppCompatResources;

/**
 * AHBottomNavigationItem
 * The item is display in the AHBottomNavigation layout
 */
public class AHBottomNavigationItem {
	
	private String title = "";
	private String tag = "";
	private Drawable drawable;
	private int color = Color.GRAY;
	
	private
	@StringRes
	int titleRes;
	private
	@DrawableRes
	int drawableRes;
	private
	@ColorRes
	int colorRes;

	/**
	 * Constructor
	 *
	 * @param titleRes    String resource
	 * @param drawableRes Drawable resource
	 * @param colorRes    Color resource
	 */
	public AHBottomNavigationItem(@StringRes int titleRes, @DrawableRes int drawableRes, @ColorRes int colorRes, String tag) {
		this.titleRes = titleRes;
		this.drawableRes = drawableRes;
		this.colorRes = colorRes;
		this.tag = tag;
	}

	public String getTitle(Context context) {
		if (titleRes != 0) {
			return context.getString(titleRes);
		}
		return title;
	}

	public String getTag() {
		return tag;
	}
	
	public void setTitle(String title) {
		this.title = title;
		this.titleRes = 0;
	}
	
	public void setTitle(@StringRes int titleRes) {
		this.titleRes = titleRes;
		this.title = "";
	}
	
	public int getColor(Context context) {
		if (colorRes != 0) {
			return ContextCompat.getColor(context, colorRes);
		}
		return color;
	}
	
	public void setColor(@ColorInt int color) {
		this.color = color;
		this.colorRes = 0;
	}

	public Drawable getDrawable(Context context) {
		if (drawableRes != 0) {
			try {
				return AppCompatResources.getDrawable(context, drawableRes);
			} catch (Resources.NotFoundException e) {
				return ContextCompat.getDrawable(context, drawableRes);
			}
		}
		return drawable;
	}
	
	public void setDrawable(@DrawableRes int drawableRes) {
		this.drawableRes = drawableRes;
		this.drawable = null;
	}
	
	public void setDrawable(Drawable drawable) {
		this.drawable = drawable;
		this.drawableRes = 0;
	}
}
