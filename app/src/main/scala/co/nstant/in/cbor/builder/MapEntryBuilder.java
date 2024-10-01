package co.nstant.in.cbor.builder;

import co.nstant.in.cbor.model.DataItem;

public class MapEntryBuilder<T extends MapBuilder<?>> extends AbstractBuilder<T> {
    private final DataItem key;

    public MapEntryBuilder(T parent, DataItem key) {
        super(parent);
        this.key = key;
    }

    public T value(boolean value) {
        return put(key, convert(value));
    }

    public T value(byte[] value) {
        return put(key, convert(value));
    }

    public T value(double value) {
        return put(key, convert(value));
    }

    public T value(String value) {
        return put(key, convert(value));
    }

    private T put(DataItem key, DataItem value) {
        getParent().put(key, value);
        return getParent();
    }

    public MapEntryBuilder<T> tagged(long tag) {
        DataItem item = key.getOuterTaggable();
        item.setTag(tag);
        return this;
    }
}
