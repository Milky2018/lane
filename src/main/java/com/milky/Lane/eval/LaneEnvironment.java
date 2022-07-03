package com.milky.Lane.eval;

import com.milky.Lane.exception.LaneBug;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.type.LaneType;

import java.util.HashMap;
import java.util.Map;

sealed public interface LaneEnvironment {
    LaneValue lookup(String var) throws LaneException;
    void extend(String var, LaneBinding binding) throws LaneException;
    void mutate(String var, LaneValue val) throws LaneException;

    final class EmptyEnvironment implements LaneEnvironment {
        @Override
        public LaneValue lookup(String var) throws LaneException {
            throw new LaneBug("Name \"" + var + "\" not found");
        }

        @Override
        public void extend(String var, LaneBinding binding) throws LaneException {
            throw new LaneBug("Empty environment cannot be extended");
        }

        @Override
        public void mutate(String var, LaneValue val) throws LaneException {
            throw new LaneBug("Variable not found, cannot be mutated: " + var);
        }
    }

    final class InnerEnvironment implements LaneEnvironment {
        private final Map<String, LaneBinding> bindingMap;
        private final LaneEnvironment outer;

        public InnerEnvironment(LaneEnvironment outer) {
            this.bindingMap = new HashMap<>();
            this.outer = outer;
        }

        public LaneValue lookup(String var) throws LaneException {
            var bind = bindingMap.get(var);
            if (bind == null) {
                return outer.lookup(var);
            } else {
                return switch (bind) {
                    case LaneBinding.ValueBinding valueBinding -> valueBinding.value();
                    case LaneBinding.FunctionBinding functionBinding -> functionBinding.definition().evaluate(this);
                };
            }
        }

        public void extend(String var, LaneBinding binding) throws LaneException {
            if (bindingMap.containsKey(var)) {
                throw new LaneBug("Variable cannot be bound repeatedly: " + var);
            }
            this.bindingMap.put(var, binding);
        }

        public void mutate(String var, LaneValue val) throws LaneException {
            var old = this.bindingMap.get(var);
            if (old == null) {
               outer.mutate(var, val);
               return;
            }
            if (old instanceof LaneBinding.ValueBinding) {
                this.bindingMap.put(var, new LaneBinding.ValueBinding(val));
            } else {
                throw new LaneBug("Variable is a not a common local binding: " + var);
            }
        }
    }
}
