package fr.acinq.eclair.router;

import java.util.HashMap;

public class DefaultHashMap<K,V> extends HashMap<K,V> {
    private final V defaultValue;

    public DefaultHashMap(V defaultValue, int initialCapacity) {
        super(initialCapacity);
        this.defaultValue = defaultValue;
    }

    public V getOrDefaultValue(Object k) {
        return containsKey(k) ? super.get(k) : defaultValue;
    }
}