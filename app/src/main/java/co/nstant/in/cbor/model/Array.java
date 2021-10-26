package co.nstant.in.cbor.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Array extends ChunkableDataItem {

    private final ArrayList<DataItem> objects;

    public Array() {
        super(MajorType.ARRAY);
        objects = new ArrayList<>();
    }

    public Array(int initialCapacity) {
        super(MajorType.ARRAY);
        objects = new ArrayList<>(initialCapacity);
    }

    public Array add(DataItem object) {
        objects.add(object);
        return this;
    }

    public List<DataItem> getDataItems() {
        return objects;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Array) {
            Array other = (Array) object;
            return super.equals(object) && objects.equals(other.objects);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ objects.hashCode();
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("[");
        if (isChunked()) {
            stringBuilder.append("_ ");
        }
        stringBuilder.append(Arrays.toString(objects.toArray()).substring(1));
        return stringBuilder.toString();
    }

    public DataItem peekLast() {
        if (objects.isEmpty()) {
            return null;
        }
        return objects.get(objects.size() - 1);
    }
}
