package eu.livotov.labs.android.camview;

import android.annotation.TargetApi;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.ImageFormat;
import android.hardware.Camera;
import android.os.Build;
import android.util.AttributeSet;
import android.util.Log;
import android.view.*;
import android.widget.FrameLayout;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * (c) Livotov Labs Ltd. 2012
 * Date: 06/12/2013
 */
public class CAMView extends FrameLayout implements SurfaceHolder.Callback, Camera.PreviewCallback
{

    private SurfaceHolder surfaceHolder;
    private SurfaceView surface;
    private Camera camera;
    private int previewFormat = ImageFormat.NV21;
    private int cameraId = -1;
    private Camera.PreviewCallback previewCallback;
    private AutoFocusManager autoFocusManager;

    private CAMViewListener camViewListener;

    private transient boolean live = false;
    private int lastUsedCameraId = -1;


    public CAMView(final Context context)
    {
        super(context);
        initUI();
    }

    public CAMView(final Context context, final AttributeSet attrs)
    {
        super(context, attrs);
        initUI();
    }

    public CAMView(final Context context, final AttributeSet attrs, final int defStyle)
    {
        super(context, attrs, defStyle);
    }

    private void initUI()
    {
    }

    public void setCamViewListener(final CAMViewListener camViewListener)
    {
        this.camViewListener = camViewListener;
    }

    @TargetApi(9)
    public static int getNumberOfCameras()
    {
        return Camera.getNumberOfCameras();
    }

    @TargetApi(9)
    public static Collection<CameraEnumeration> enumarateCameras()
    {

        if (Build.VERSION.SDK_INT < 9)
        {
            throw new UnsupportedOperationException("Camera enumeration is only available for Android SDK version 9 and above.");
        }


        List<CameraEnumeration> cameras = new ArrayList<CameraEnumeration>();

        final int camerasCount = Camera.getNumberOfCameras();

        for (int id = 0; id < camerasCount; id++)
        {
            final Camera.CameraInfo cameraInfo = new Camera.CameraInfo();
            Camera.getCameraInfo(id, cameraInfo);
            cameras.add(new CameraEnumeration(id, cameraInfo));
        }

        return cameras;
    }

    public static CameraEnumeration findFrontCamera()
    {
        Collection<CameraEnumeration> cams = enumarateCameras();

        for (CameraEnumeration cam : cams)
        {
            if (cam.isFrontCamera())
            {
                return cam;
            }
        }
        return null;
    }

    public synchronized void stop()
    {

        live = false;

        if (camera != null)
        {
            camera.setPreviewCallback(null);
            camera.release();
            camera = null;
        }

        if (surfaceHolder != null)
        {
            surfaceHolder.removeCallback(this);
        }
    }

    public void start()
    {
        start(findDefaultCameraId());
    }

    public synchronized void start(final int cameraId)
    {
        this.cameraId = cameraId;
        this.camera = setupCamera(cameraId);
        previewCallback = this;


        try
        {
            Camera.Parameters parameters = getMainCameraParameters();
            parameters.setPreviewFormat(previewFormat);
            camera.setParameters(parameters);
        } catch (Throwable err)
        {
            Log.e(getClass().getSimpleName(), "Master parameters set was rejected by a camera, trying failsafe one.", err);

            try
            {
                Camera.Parameters parameters = getFailsafeCameraParameters();
                parameters.setPreviewFormat(previewFormat);
                camera.setParameters(parameters);
            } catch (Throwable err2)
            {
                Log.e(getClass().getSimpleName(), "Failsafe parameters set was rejected by a camera, trying to use it as is.", err2);
            }
        }

        removeAllViews();
        surface = new SurfaceView(getContext());
        addView(surface);

        FrameLayout.LayoutParams params = (FrameLayout.LayoutParams) surface.getLayoutParams();
        params.gravity = Gravity.CENTER;
        params.width = LayoutParams.MATCH_PARENT;
        params.height = LayoutParams.MATCH_PARENT;
        surface.setLayoutParams(params);

        surfaceHolder = surface.getHolder();
        surfaceHolder.addCallback(this);

        if (Build.VERSION.SDK_INT < 11)
        {
            try
            {
                // deprecated setting, but required on Android versions prior to 3.0
                surfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
            } catch (Throwable err)
            {
                Log.e(getClass().getSimpleName(), "Failed to set surface holder to SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS, using it as is.", err);
            }
        }

        lastUsedCameraId = cameraId;
        live = true;
    }

    public void surfaceCreated(SurfaceHolder holder)
    {
        try
        {
            camera.setPreviewDisplay(holder);
        } catch (Throwable e)
        {
            Log.d("DBG", "Error setting camera preview: " + e.getMessage());
        }
    }

    public void surfaceDestroyed(SurfaceHolder holder)
    {
    }

    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
    {
        if (surfaceHolder.getSurface() == null)
        {
            return;
        }

        stopPreview();

        try
        {
            Camera.Parameters p = camera.getParameters();


            int result = 90;
            int outputResult = 90;


            if (Build.VERSION.SDK_INT > 8)
            {
                int[] results = calculateResults(result, outputResult);
                result = results[0];
                outputResult = results[1];
            }

            if (Build.VERSION.SDK_INT > 7)
            {
                try
                {
                    camera.setDisplayOrientation(result);
                } catch (Throwable err)
                {
                    // very bad devices goes here
                }
            }

            p.setRotation(outputResult);
            camera.setPreviewDisplay(surfaceHolder);
            camera.setPreviewCallback(previewCallback);

            Camera.Size closestSize = findClosestPreviewSize(p.getSupportedPreviewSizes());

            if (closestSize != null)
            {
                FrameLayout.LayoutParams params = (FrameLayout.LayoutParams) surface.getLayoutParams();
                params.gravity = Gravity.CENTER;
                params.width = (getWidth() > getHeight() ? closestSize.width : closestSize.height);
                params.height = (getWidth() > getHeight() ? closestSize.height : closestSize.width);
                surface.setLayoutParams(params);

                if (params.width < getWidth() || params.height < getHeight())
                {
                    final int extraPixels = Math.max(getWidth() - params.width, getHeight() - params.height);
                    params.width += extraPixels;
                    params.height += extraPixels;
                }

                p.setPreviewSize(closestSize.width, closestSize.height);
            }

            camera.setParameters(p);
            startPreview();

        } catch (Exception e)
        {
            Log.d("DBG", "Error starting camera preview: " + e.getMessage());
        }
    }

    public void stopPreview()
    {
        if (null != camera)
        {
            if (autoFocusManager != null)
            {
                autoFocusManager.stop();
                autoFocusManager = null;
            }

            try
            {
                camera.stopPreview();
            } catch (Exception ignored)
            {
            }
        }
    }

    private void startPreview()
    {
        if (null != camera)
        {
            camera.startPreview();
            autoFocusManager = new AutoFocusManager(getContext(), camera);
        }
    }

    @TargetApi(Build.VERSION_CODES.GINGERBREAD)
    private int[] calculateResults(int _result, int _outputResult)
    {
        int result = _result;
        int outputResult = _outputResult;

        try
        {
            android.hardware.Camera.CameraInfo info = new android.hardware.Camera.CameraInfo();
            android.hardware.Camera.getCameraInfo(cameraId, info);

            int rotation = ((WindowManager) getContext().getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay().getRotation();
            int degrees = 0;

            switch (rotation)
            {
                case Surface.ROTATION_0:
                    degrees = 0;
                    break;
                case Surface.ROTATION_90:
                    degrees = 90;
                    break;
                case Surface.ROTATION_180:
                    degrees = 180;
                    break;
                case Surface.ROTATION_270:
                    degrees = 270;
                    break;
            }

            if (info.facing == Camera.CameraInfo.CAMERA_FACING_FRONT)
            {
                result = (info.orientation + degrees) % 360;
                outputResult = (info.orientation + degrees) % 360;
                result = (360 - result) % 360;  // compensate the mirror
            } else
            {  // back-facing
                result = (info.orientation - degrees + 360) % 360;
            }
        } catch (Throwable err)
        {
            // very bad devices goes here
        }
        return new int[]{result, outputResult};
    }


    private Camera.Size findClosestPreviewSize(List<Camera.Size> sizes)
    {
        int best = -1;
        int bestScore = Integer.MAX_VALUE;

        for (int i = 0; i < sizes.size(); i++)
        {
            Camera.Size s = sizes.get(i);

            int dx = s.width - surface.getWidth();
            int dy = s.height - surface.getHeight();

            int score = dx * dx + dy * dy;
            if (score < bestScore)
            {
                best = i;
                bestScore = score;
            }
        }

        return best >= 0 ? sizes.get(best) : null;
    }

    protected void onConfigurationChanged(final Configuration newConfig)
    {
        super.onConfigurationChanged(newConfig);

        if (live)
        {
            postDelayed(new Runnable()
            {
                public void run()
                {
                    stop();
                    postDelayed(new Runnable()
                    {
                        public void run()
                        {
                            try
                            {
                                if (lastUsedCameraId >= 0)
                                {
                                    start(lastUsedCameraId);
                                } else
                                {
                                    start();
                                }
                            } catch (Throwable err)
                            {
                                Log.e(getClass().getSimpleName(),"Failed to re-open the camera after configuration change: " + err.getMessage(),err);
                            }
                        }
                    }, 100);
                }
            }, 50);
        }
    }

    public void onPreviewFrame(byte[] data, Camera camera)
    {
        if (camViewListener != null)
        {
            try
            {
                Camera.Size s = camera.getParameters().getPreviewSize();
                camViewListener.onPreviewData(data, s.width, s.height);
            } catch (Throwable ignored)
            {
            }
        }
    }


    //private static final long AUTO_FOCUS_INTERVAL_MS = 1000L;
    //private static final long AUTO_FOCUS_INTERVAL_MS = 2000L;

    protected int findDefaultCameraId()
    {
        if (Build.VERSION.SDK_INT < 9)
        {
            return 0;
        } else
        {
            return findCamera();
        }

    }

    @TargetApi(Build.VERSION_CODES.GINGERBREAD)
    private int findCamera()
    {
        final int camerasCount = Camera.getNumberOfCameras();
        final Camera.CameraInfo cameraInfo = new Camera.CameraInfo();

        for (int id = 0; id < camerasCount; id++)
        {
            Camera.getCameraInfo(id, cameraInfo);

            if (cameraInfo.facing == Camera.CameraInfo.CAMERA_FACING_BACK)
            {
                return id;
            }
        }
        if (camerasCount > 0)
        {
            return 0;
        }

        //throw new RuntimeException("Did not find back camera on this device");
        throw new RuntimeException("Did not find camera on this device");
    }

    protected Camera setupCamera(final int cameraId)
    {
        try
        {
            if (Build.VERSION.SDK_INT < 9)
            {
                return Camera.open();
            } else
            {
                return openCamera(cameraId);
            }
        } catch (Throwable e)
        {
            throw new RuntimeException("Failed to open a camera with id " + cameraId + ": " + e.getMessage(), e);
        }
    }

    @TargetApi(Build.VERSION_CODES.GINGERBREAD)
    private Camera openCamera(final int cameraId)
    {
        return Camera.open(cameraId);
    }


    private String findSettableValue(Collection<String> supportedValues, String... desiredValues)
    {
        //Log.i(TAG, "Supported values: " + supportedValues);
        String result = null;
        if (supportedValues != null)
        {
            for (String desiredValue : desiredValues)
            {
                if (supportedValues.contains(desiredValue))
                {
                    result = desiredValue;
                    break;
                }
            }
        }
        return result;
    }


    private Camera.Parameters getMainCameraParameters()
    {
        Camera.Parameters parameters = camera.getParameters();

        if (Build.VERSION.SDK_INT >= 9)
        {
            setFocusMode(parameters);
        }

        if (Build.VERSION.SDK_INT > 13)
        {
            setAutoExposureLock(parameters);
        }

        if (Build.VERSION.SDK_INT > 7)
        {
            try
            {
                if (parameters.getMaxExposureCompensation() != 0 || parameters.getMinExposureCompensation() != 0)
                {
                    parameters.setExposureCompensation(0);
                }
            } catch (Throwable ignored)
            {
            }
        }

        return parameters;
    }

    @TargetApi(14)
    private String getFocusMode14(List<String> focusModes)
    {

        boolean safeMode = false;

        if (safeMode
            // || prefs.getBoolean(PreferencesActivity.KEY_DISABLE_CONTINUOUS_FOCUS, true)
                )
        {
            return findSettableValue(focusModes, Camera.Parameters.FOCUS_MODE_AUTO);
        } else
        {
            return findSettableValue(focusModes,
                                     Camera.Parameters.FOCUS_MODE_CONTINUOUS_PICTURE,
                                     Camera.Parameters.FOCUS_MODE_CONTINUOUS_VIDEO,
                                     Camera.Parameters.FOCUS_MODE_AUTO);
        }
    }

    @TargetApi(9)
    private String getFocusMode9(List<String> focusModes)
    {
        return findSettableValue(focusModes, Camera.Parameters.FOCUS_MODE_AUTO);
    }

    @TargetApi(9)
    private void setFocusMode(Camera.Parameters parameters)
    {
        String focusMode;

        List<String> focusModes = parameters.getSupportedFocusModes();

        if (Build.VERSION.SDK_INT >= 14)
        {
            focusMode = getFocusMode14(focusModes);
        } else
        {
            focusMode = getFocusMode9(focusModes);
        }

        if (null == focusMode)
        {
            focusMode = findSettableValue(focusModes,
                                          Camera.Parameters.FOCUS_MODE_MACRO,
                                          Camera.Parameters.FOCUS_MODE_EDOF);
        }

        if (null != focusMode)
        {
            parameters.setFocusMode(focusMode);
        }
    }

    @TargetApi(Build.VERSION_CODES.ICE_CREAM_SANDWICH)
    private void setAutoExposureLock(Camera.Parameters parameters)
    {
        try
        {
            if (parameters.isAutoExposureLockSupported())
            {
                parameters.setAutoExposureLock(false);
            }
        } catch (Throwable ignored)
        {
        }
    }

    private Camera.Parameters getFailsafeCameraParameters()
    {
        Camera.Parameters parameters = camera.getParameters();

        if (Build.VERSION.SDK_INT >= 9)
        {
            setFocusMode(parameters);
        }
        return parameters;
    }

    public boolean isStreaming()
    {
        return camera != null;
    }

    public Camera getCamera()
    {
        return camera;
    }

    public interface CAMViewListener
    {
        void onPreviewData(byte[] data, int width, int height);
    }
}