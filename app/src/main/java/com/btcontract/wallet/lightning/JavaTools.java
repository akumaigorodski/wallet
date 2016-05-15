package com.btcontract.wallet.lightning;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.IOException;


public class JavaTools {
    public static Long uint64(int a, int b, int c, int d, int e, int f, int g, int h) {
        return (a & 0xffL) | ((b & 0xffL) << 8) | ((c & 0xffL) << 16) | ((d & 0xffL) << 24) |
                ((e & 0xffL) << 32) | ((f & 0xffL) << 40) | ((g & 0xffL) << 48) | ((h & 0xffL) << 56);
    }

    public static Long uint64(InputStream input) throws IOException {
        return uint64(input.read(), input.read(), input.read(), input.read(),
                input.read(), input.read(), input.read(), input.read());
    }

    public static void writeUInt8(OutputStream out, Long... inputs) throws IOException {
        for (Long input : inputs) out.write( (int)(input & 0xff) );
    }

    public static void writeUInt64(OutputStream out, Long... inputs) throws IOException {
        for (Long input : inputs) writeUInt8(out, input & 0xff, (input >>> 8) & 0xff,
                (input >>> 16) & 0xff, (input >>> 24) & 0xff, (input >>> 32) & 0xff,
                (input >>> 40) & 0xff, (input >>> 48) & 0xff, (input >>> 56) & 0xff);
    }

    public static byte[] writeUInt64(long input) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream(8);
        writeUInt64(output, input);
        return output.toByteArray();
    }

    public static byte[] concat(byte[]... arrays) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream(8);
        for (byte[] array : arrays) output.write(array);
        return output.toByteArray();
    }
}
