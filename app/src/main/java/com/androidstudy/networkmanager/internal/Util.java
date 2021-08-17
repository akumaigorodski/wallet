package com.androidstudy.networkmanager.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by chweya on 29/08/17.
 */

public class Util {
    private Util() {
    }

    public static <T> List<T> getSnapshot(Collection<T> other) {
        // toArray creates a new ArrayList internally and this way we can guarantee entries will not
        // be null. See #322.
        List<T> result = new ArrayList<>(other.size());
        result.addAll(other);
        return result;
    }
}
