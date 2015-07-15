package eu.livotov.labs.android.camview;

import android.annotation.TargetApi;
import android.hardware.Camera;

/**
 * (c) Livotov Labs Ltd. 2014
 * This is the simple utility object to hold most important device camera parameters in a bit more developer friendly way.
 */
public class CameraEnumeration
{

    private int cameraId;
    private boolean frontCamera;
    private Camera.CameraInfo cameraInfo;

    @TargetApi(11)
    public CameraEnumeration(final int cameraId, final Camera.CameraInfo cameraInfo)
    {
        this.cameraId = cameraId;
        this.cameraInfo = cameraInfo;
        frontCamera = cameraInfo.facing == Camera.CameraInfo.CAMERA_FACING_FRONT;
    }

    public int getCameraId()
    {
        return cameraId;
    }

    public Camera.CameraInfo getCameraInfo()
    {
        return cameraInfo;
    }

    public boolean isFrontCamera()
    {
        return frontCamera;
    }
}