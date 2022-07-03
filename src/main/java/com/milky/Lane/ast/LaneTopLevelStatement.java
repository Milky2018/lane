package com.milky.Lane.ast;

public sealed interface LaneTopLevelStatement {
    String prettyPrint();

    record LaneNamedFunctionDefinition(String functionName, LaneFunctionDefinition functionDefinition) implements LaneTopLevelStatement {
        @Override
        public String prettyPrint() {
            return "func " + functionName + ": \n" + functionDefinition.prettyPrint();
        }
    }
}
