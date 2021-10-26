package co.nstant.in.cbor.model;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

public class Map extends ChunkableDataItem {

    private final LinkedHashMap<DataItem, DataItem> map;
    private final List<DataItem> keys = new LinkedList<>();

    public Map() {
        super(MajorType.MAP);
        map = new LinkedHashMap<>();
    }

    public Map(int initialCapacity) {
        super(MajorType.MAP);
        map = new LinkedHashMap<>(initialCapacity);
    }

    public Map put(DataItem key, DataItem value) {
        if (map.put(key, value) == null) {
            keys.add(key);
        }
        return this;
    }

    public DataItem get(DataItem key) {
        return map.get(key);
    }

    public DataItem remove(DataItem key) {
        keys.remove(key);
        return map.remove(key);
    }

    public Collection<DataItem> getKeys() {
        return keys;
    }

    public Collection<DataItem> getValues() {
        return map.values();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Map) {
            Map other = (Map) object;
            return super.equals(object) && map.equals(other.map);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ map.hashCode();
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        if (isChunked()) {
            stringBuilder.append("{_ ");
        } else {
            stringBuilder.append("{ ");
        }
        for (DataItem key : keys) {
            stringBuilder.append(key).append(": ").append(map.get(key)).append(", ");
        }
        if (stringBuilder.toString().endsWith(", ")) {
            stringBuilder.setLength(stringBuilder.length() - 2);
        }
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }

}
