package com.milky.Lane.type;

import java.util.List;

public interface LaneType {
    boolean acceptType(LaneType type);
    record StringType() implements LaneType {
        @Override
        public boolean acceptType(LaneType type) {
            return type instanceof StringType;
        }
    }
    record NumberType() implements LaneType {
        @Override
        public boolean acceptType(LaneType type) {
            return type instanceof NumberType;
        }
    }
    record BoolType() implements LaneType {
        @Override
        public boolean acceptType(LaneType type) {
            return type instanceof BoolType;
        }
    }
    record UnitType() implements LaneType {
        @Override
        public boolean acceptType(LaneType type) {
            return type instanceof UnitType;
        }
    }
    record FunctionType(List<LaneType> argsType, LaneType retType) implements LaneType {
        @Override
        public boolean acceptType(LaneType type) {
            if (type instanceof FunctionType functionType && retType.acceptType(functionType.retType)) {
                var funcArgsType = functionType.argsType;
                if (funcArgsType.size() == argsType.size()) {
                    for (int i = 0; i < argsType.size(); i++) {
                        var argType = argsType.get(i);
                        var target = funcArgsType.get(i);
                        if (!argType.acceptType(target)) {
                            return false;
                        }
                    }
                    return true;
                }
                return false;
            }
            return false;
        }
    }
}
