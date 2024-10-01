package co.nstant.in.cbor.builder;

import co.nstant.in.cbor.model.Array;
import co.nstant.in.cbor.model.DataItem;
import co.nstant.in.cbor.model.Map;
import co.nstant.in.cbor.model.SimpleValue;

public class ArrayBuilder<T extends AbstractBuilder<?>> extends AbstractBuilder<T> {

    private final Array array;

    public ArrayBuilder(T parent, Array array) {
        super(parent);
        this.array = array;
    }

    public ArrayBuilder<T> add(DataItem dataItem) {
        array.add(dataItem);
        return this;
    }

    public ArrayBuilder<T> add(long value) {
        add(convert(value));
        return this;
    }

    public ArrayBuilder<T> add(boolean value) {
        add(convert(value));
        return this;
    }

    public ArrayBuilder<T> add(float value) {
        add(convert(value));
        return this;
    }

    public ArrayBuilder<T> add(double value) {
        add(convert(value));
        return this;
    }

    public ArrayBuilder<T> add(byte[] bytes) {
        add(convert(bytes));
        return this;
    }

    public ArrayBuilder<T> add(String string) {
        add(convert(string));
        return this;
    }

    public ArrayBuilder<T> tagged(long tag) {
        DataItem item = array.peekLast();
        if (item == null) {
            throw new IllegalStateException("Can't add a tag before adding an item");
        }
        item.getOuterTaggable().setTag(tag);
        return this;
    }

    public ArrayBuilder<ArrayBuilder<T>> addArray() {
        Array nestedArray = new Array();
        add(nestedArray);
        return new ArrayBuilder<ArrayBuilder<T>>(this, nestedArray);
    }

    public ArrayBuilder<ArrayBuilder<T>> startArray() {
        Array nestedArray = new Array();
        nestedArray.setChunked(true);
        add(nestedArray);
        return new ArrayBuilder<ArrayBuilder<T>>(this, nestedArray);
    }

    public MapBuilder<ArrayBuilder<T>> addMap() {
        Map nestedMap = new Map();
        add(nestedMap);
        return new MapBuilder<ArrayBuilder<T>>(this, nestedMap);
    }

    public MapBuilder<ArrayBuilder<T>> startMap() {
        Map nestedMap = new Map();
        nestedMap.setChunked(true);
        add(nestedMap);
        return new MapBuilder<ArrayBuilder<T>>(this, nestedMap);
    }

    public T end() {
        if (array.isChunked()) {
            add(SimpleValue.BREAK);
        }
        return getParent();
    }

}
