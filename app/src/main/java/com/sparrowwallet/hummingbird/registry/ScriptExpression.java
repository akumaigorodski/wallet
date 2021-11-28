package com.sparrowwallet.hummingbird.registry;

public enum ScriptExpression {
    SCRIPT_HASH(400, "sh"),
    WITNESS_SCRIPT_HASH(401, "wsh"),
    PUBLIC_KEY(402, "pk"),
    PUBLIC_KEY_HASH(403, "pkh"),
    WITNESS_PUBLIC_KEY_HASH(404, "wpkh"),
    COMBO(405, "combo"),
    MULTISIG(406, "multi"),
    SORTED_MULTISIG(407, "sorted"),
    ADDRESS(307, "addr"),
    RAW_SCRIPT(408, "raw"),
    TAPROOT(409, "tr");

    private final int tagValue;
    private final String expression;

    private ScriptExpression(int tagValue, String expression) {
        this.tagValue = tagValue;
        this.expression = expression;
    }

    public int getTagValue() {
        return tagValue;
    }

    public String getExpression() {
        return expression;
    }

    public static ScriptExpression fromTagValue(long value) {
        for(ScriptExpression expression : ScriptExpression.values()) {
            if(expression.tagValue == value) {
                return expression;
            }
        }

        throw new IllegalArgumentException("Unknown tag value " + value);
    }
}
