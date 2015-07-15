package eu.livotov.labs.android.camview;

import android.os.Looper;
import android.os.Message;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("unused")
public abstract class CAMViewAsyncTask<Params, Progress, Result>
{

    private static final int CPU_COUNT = Runtime.getRuntime().availableProcessors();
    private static final int CORE_POOL_SIZE = CPU_COUNT + 1;
    private static final int MAXIMUM_POOL_SIZE = CPU_COUNT * 2 + 1;
    private static final int KEEP_ALIVE = 1;

    private static final BlockingQueue<Runnable> sPoolWorkQueue = new LinkedBlockingQueue<Runnable>(128);
    private final static Runloop sRunLoop = new Runloop();

    private static final ThreadFactory sThreadFactory = new ThreadFactory()
    {
        private final AtomicInteger mCount = new AtomicInteger(1);

        public Thread newThread(Runnable r)
        {
            return new Thread(r, "AsyncTask #" + mCount.getAndIncrement());
        }
    };

    public static final Executor sPool = new ThreadPoolExecutor(CORE_POOL_SIZE, MAXIMUM_POOL_SIZE, KEEP_ALIVE, TimeUnit.SECONDS, sPoolWorkQueue, sThreadFactory);
    private static final Handler sHandler = new Handler();
    private final Object mLock = new Object();
    private AtomicBoolean mCancelled = new AtomicBoolean(false);

    protected void onPreExecute()
    {

    }

    protected abstract Result doInBackground(Params... args) throws Throwable;

    protected void onProgressUpdate(Progress progress)
    {

    }

    protected void onPostExecute(Result result)
    {

    }

    protected void onError(Throwable t)
    {

    }

    protected void onCanceled(Result result)
    {

    }

    public void publishProgress(Progress progress)
    {
        if (!mCancelled.get())
        {
            AsyncResult<Progress, Result> result = new AsyncResult<Progress, Result>(this);
            result.progress = progress;
            sHandler.dispatchProgressUpdate(result);
        }
    }

    public void cancel()
    {
        mCancelled.set(true);
    }

    public boolean isCanceled()
    {
        return mCancelled.get();
    }

    public void execPool(final Params... params)
    {
        sPool.execute(new Runnable()
        {
            @Override
            public void run()
            {
                execInCurrThread(params);
            }
        });
    }

    public void execSerial(final Params... params)
    {
        if (!sRunLoop.isStarted())
        {
            sRunLoop.start();
        }
        sRunLoop.post(new Runnable()
        {
            @Override
            public void run()
            {
                execInCurrThread(params);
            }
        });
    }

    public void execInCurrThread(Params... params)
    {
        synchronized (mLock)
        {
            AsyncResult<Progress, Result> result = new AsyncResult<Progress, Result>(this);
            if (!mCancelled.get())
            {
                sHandler.dispatchPreExecute(result);
                if (!mCancelled.get())
                {
                    try
                    {
                        result.result = doInBackground(params);
                        if (!mCancelled.get())
                        {
                            sHandler.dispatchPostExecute(result);
                        } else
                        {
                            sHandler.dispatchCancel(result);
                        }
                    } catch (Throwable throwable)
                    {
                        if (!mCancelled.get())
                        {
                            sHandler.dispatchError(result);
                        } else
                        {
                            sHandler.dispatchCancel(result);
                        }
                    }
                } else
                {
                    sHandler.dispatchCancel(result);
                }
            } else
            {
                sHandler.dispatchCancel(result);
            }
        }
    }

    static class Handler extends android.os.Handler
    {

        static final int MESSAGE_PRE_EXECUTE = 1;
        static final int MESSAGE_PROGRESS = 2;
        static final int MESSAGE_ERROR = 3;
        static final int MESSAGE_CANCEL = 4;
        static final int MESSAGE_POST_EXECUTE = 5;

        Handler()
        {
            super(Looper.getMainLooper());
        }

        void dispatchPreExecute(AsyncResult result)
        {
            sendMessage(result, MESSAGE_PRE_EXECUTE);
        }

        void dispatchProgressUpdate(AsyncResult result)
        {
            sendMessage(result, MESSAGE_PROGRESS);
        }

        void dispatchError(AsyncResult result)
        {
            sendMessage(result, MESSAGE_ERROR);
        }

        void dispatchCancel(AsyncResult result)
        {
            sendMessage(result, MESSAGE_CANCEL);
        }

        void dispatchPostExecute(AsyncResult result)
        {
            sendMessage(result, MESSAGE_POST_EXECUTE);
        }

        void sendMessage(AsyncResult result, int code)
        {
            Message m = obtainMessage(code);
            m.obj = result;
            m.sendToTarget();
        }

        @Override
        @SuppressWarnings("unchecked")
        public void handleMessage(Message msg)
        {
            AsyncResult obj = (AsyncResult) msg.obj;
            if (obj != null)
            {
                switch (msg.what)
                {
                    case MESSAGE_PRE_EXECUTE:
                        obj.task.onPreExecute();
                        break;
                    case MESSAGE_PROGRESS:
                        obj.task.onProgressUpdate(obj.progress);
                        break;
                    case MESSAGE_ERROR:
                        obj.task.onError(obj.t);
                        break;
                    case MESSAGE_CANCEL:
                        obj.task.onCanceled(obj.result);
                        break;
                    case MESSAGE_POST_EXECUTE:
                        obj.task.onPostExecute(obj.result);
                        break;
                }
            }
        }
    }

    static class AsyncResult<Progress, Result>
    {

        final CAMViewAsyncTask task;

        Throwable t;
        Progress progress;
        Result result;

        AsyncResult(CAMViewAsyncTask task)
        {
            this.task = task;
        }
    }

    static class Runloop extends Thread
    {

        private final Object mLock = new Object();
        private android.os.Handler mHandler;
        private boolean mReady = false;

        @Override
        public void run()
        {
            Looper.prepare();
            mHandler = new android.os.Handler();
            synchronized (mLock)
            {
                this.mLock.notifyAll();
                mReady = true;
            }
            Looper.loop();
        }

        public void sendMessage(Message message)
        {
            sendMessage(message, 0);
        }

        public void post(Runnable runnable)
        {
            post(runnable, 0);
        }

        public void sendMessage(Message message, long delayMillis)
        {
            synchronized (mLock)
            {
                while (!mReady)
                {
                    try
                    {
                        mLock.wait();
                    } catch (InterruptedException e)
                    {
                        e.printStackTrace();
                    }
                }
                if (mHandler != null)
                {
                    if (delayMillis <= 0)
                    {
                        this.mHandler.sendMessage(message);
                    } else
                    {
                        mHandler.sendMessageDelayed(message, delayMillis);
                    }
                }
            }
        }

        public void post(Runnable runnable, long delayMillis)
        {
            synchronized (mLock)
            {

                while (!mReady)
                {
                    try
                    {
                        mLock.wait();
                    } catch (InterruptedException e)
                    {
                        e.printStackTrace();
                    }
                }

                if (mHandler != null)
                {
                    if (delayMillis <= 0)
                    {
                        this.mHandler.post(runnable);
                    } else
                    {
                        mHandler.postDelayed(runnable, delayMillis);
                    }
                }
            }
        }

        public boolean isStarted()
        {
            return mReady;
        }
    }

}