package eu.livotov.zxscan.util;

import android.content.Context;
import android.media.MediaPlayer;
import android.util.Log;

/**
 * (c) Livotov Labs Ltd. 2012
 * Date: 03/11/2014
 */
public class SoundPlayer implements MediaPlayer.OnPreparedListener
{

    private MediaPlayer mPlayer;
    private Context ctx;

    public SoundPlayer(Context ctx)
    {
        this.ctx = ctx;
        initPlayer();
    }


    public void playRawResource(int rawResource, boolean loop)
    {
        stop();
        try
        {
            mPlayer = MediaPlayer.create(ctx, rawResource);
            if (loop)
            {
                mPlayer.setLooping(loop);
            }
            mPlayer.start();
        } catch (Exception ex)
        {
            Log.e(getClass().getSimpleName(), "Could not play audio file: " + ex.getMessage());
            mPlayer = null;
        }
    }

    public void stop()
    {
        if (mPlayer != null)
        {
            try
            {
                if (mPlayer.isPlaying())
                {
                    mPlayer.stop();
                }

                mPlayer.release();
                mPlayer = null;
            } catch (Throwable err)
            {
                Log.e(getClass().getSimpleName(), "EXCEPTION: " + err.getMessage());
            }
        }
    }

    private synchronized void initPlayer()
    {
    }

    @Override
    public void onPrepared(MediaPlayer mp)
    {
        try
        {
            mp.start();
        } catch (Exception ex)
        {
            stop();
        }
    }
}
