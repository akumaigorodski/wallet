package co.nstant.in.cbor.builder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;

public class MapBuilder<T extends AbstractBuilder<?>> extends AbstractBuilder<T> {

    private final Map map;
    private DataItem lastItem = null;

    public MapBuilder(T parent, Map map) {
        super(parent);
        this.map = map;
    }

    public MapBuilder<T> put(DataItem key, DataItem value) {
        map.put(key, value);
        lastItem = value;
        return this;
    }

    public MapBuilder<T> put(long key, long value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(long key, boolean value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(long key, float value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(long key, double value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(long key, byte[] value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(long key, String value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, long value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, boolean value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, float value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, double value) {
        put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, byte[] value) {
        map.put(convert(key), convert(value));
        return this;
    }

    public MapBuilder<T> put(String key, String value) {
        put(convert(key), convert(value));
        return this;
    }

    public ArrayBuilder<MapBuilder<T>> putArray(DataItem key) {
        Array array = new Array();
        put(key, array);
        return new ArrayBuilder<>(this, array);
    }

    public ArrayBuilder<MapBuilder<T>> putArray(long key) {
        Array array = new Array();
        put(convert(key), array);
        return new ArrayBuilder<>(this, array);
    }

    public ArrayBuilder<MapBuilder<T>> putArray(String key) {
        Array array = new Array();
        put(convert(key), array);
        return new ArrayBuilder<>(this, array);
    }

    public ArrayBuilder<MapBuilder<T>> startArray(DataItem key) {
        Array array = new Array();
        array.setChunked(true);
        put(key, array);
        return new ArrayBuilder<>(this, array);
    }

    public ArrayBuilder<MapBuilder<T>> startArray(long key) {
        return startArray(convert(key));
    }

    public ArrayBuilder<MapBuilder<T>> startArray(String key) {
        Array array = new Array();
        array.setChunked(true);
        put(convert(key), array);
        return new ArrayBuilder<>(this, array);
    }

    public MapBuilder<MapBuilder<T>> putMap(DataItem key) {
        Map nestedMap = new Map();
        put(key, nestedMap);
        return new MapBuilder<>(this, nestedMap);
    }

    public MapBuilder<MapBuilder<T>> putMap(long key) {
        Map nestedMap = new Map();
        put(convert(key), nestedMap);
        return new MapBuilder<>(this, nestedMap);
    }

    public MapBuilder<MapBuilder<T>> putMap(String key) {
        Map nestedMap = new Map();
        put(convert(key), nestedMap);
        return new MapBuilder<>(this, nestedMap);
    }

    public MapBuilder<MapBuilder<T>> startMap(DataItem key) {
        Map nestedMap = new Map();
        nestedMap.setChunked(true);
        put(key, nestedMap);
        return new MapBuilder<>(this, nestedMap);
    }

    public MapBuilder<T> tagged(long tag) {
        if (lastItem == null) {
            throw new IllegalStateException("Can't add a tag before adding an item");
        }
        lastItem.getOuterTaggable().setTag(tag);
        return this;
    }

    public MapBuilder<MapBuilder<T>> startMap(long key) {
        return startMap(convert(key));
    }

    public MapBuilder<MapBuilder<T>> startMap(String key) {
        return startMap(convert(key));
    }

    public T end() {
        return getParent();
    }

    public MapEntryBuilder<MapBuilder<T>> addKey(long key) {
        return new MapEntryBuilder<>(this, convert(key));
    }

    public MapEntryBuilder<MapBuilder<T>> addKey(String key) {
        return new MapEntryBuilder<>(this, convert(key));
    }
}
