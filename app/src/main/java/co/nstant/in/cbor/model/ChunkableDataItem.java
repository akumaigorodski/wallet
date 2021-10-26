package co.nstant.in.cbor.model;

import java.util.Objects;

class ChunkableDataItem extends DataItem {

    private boolean chunked = false;

    protected ChunkableDataItem(MajorType majorType) {
        super(majorType);
    }

    public boolean isChunked() {
        return chunked;
    }

    public ChunkableDataItem setChunked(boolean chunked) {
        this.chunked = chunked;
        return this;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof ChunkableDataItem) {
            ChunkableDataItem other = (ChunkableDataItem) object;
            return super.equals(object) && chunked == other.chunked;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() ^ Objects.hashCode(chunked);
    }

}
