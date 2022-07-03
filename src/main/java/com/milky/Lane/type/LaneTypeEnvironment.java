package com.milky.Lane.type;

import com.milky.Lane.exception.LaneTypeException;

import java.util.HashMap;
import java.util.Map;

sealed public interface LaneTypeEnvironment {
    LaneType lookup(String name) throws LaneTypeException;
    void extend(String var, LaneType type, boolean mutable) throws LaneTypeException;
    void mutate(String var, LaneType type) throws LaneTypeException;

    final class EmptyTypeEnvironment implements LaneTypeEnvironment {
        @Override
        public LaneType lookup(String name) throws LaneTypeException {
            throw new LaneTypeException("Empty type environment cannot be looked up");
        }

        @Override
        public void extend(String var, LaneType type, boolean mutable) throws LaneTypeException {
            throw new LaneTypeException("Empty type environment cannot be extended");
        }

        @Override
        public void mutate(String var, LaneType type) throws LaneTypeException {
            throw new LaneTypeException("Empty type environment cannot be mutated");
        }
    }

    final class InnerTypeEnvironment implements LaneTypeEnvironment {
        private final Map<String, LaneTypeBinding> typeMap;
        private final LaneTypeEnvironment outer;

        public InnerTypeEnvironment(LaneTypeEnvironment outer) {
            this.outer = outer;
            this.typeMap = new HashMap<>();
        }

        @Override
        public LaneType lookup(String name) throws LaneTypeException {
            var type = this.typeMap.get(name);
            if (type == null) {
                return this.outer.lookup(name);
            } else {
                return type.type();
            }
        }

        @Override
        public void extend(String var, LaneType type, boolean mutable) throws LaneTypeException {
            if (this.typeMap.containsKey(var)) {
                throw new LaneTypeException("Variable cannot be bound twice: " + var);
            } else {
                this.typeMap.put(var, new LaneTypeBinding(type, mutable));
            }
        }

        @Override
        public void mutate(String var, LaneType type) throws LaneTypeException {
            var oldType = this.typeMap.get(var);
            if (oldType == null) {
                this.outer.mutate(var, type);
            } else if (oldType.mutable()) {
                this.typeMap.put(var, new LaneTypeBinding(type, true));
            } else {
                throw new LaneTypeException("Variable not mutable: " + var);
            }
        }
    }
}
