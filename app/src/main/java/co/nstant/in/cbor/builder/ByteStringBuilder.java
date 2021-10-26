package co.nstant.in.cbor.builder;

import co.nstant.in.cbor.model.SimpleValue;

public class ByteStringBuilder<T extends AbstractBuilder<?>> extends AbstractBuilder<T> {

    public ByteStringBuilder(T parent) {
        super(parent);
    }

    public ByteStringBuilder<T> add(byte[] bytes) {
        getParent().addChunk(convert(bytes));
        return this;
    }

    public T end() {
        getParent().addChunk(SimpleValue.BREAK);
        return getParent();
    }

}
