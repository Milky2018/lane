package com.milky.Lane.builtin;

import com.milky.Lane.exception.LaneBug;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.eval.LaneBinding;
import com.milky.Lane.eval.LaneEnvironment;
import com.milky.Lane.eval.LaneValue;

import java.util.List;

sealed public interface LaneBuiltinFunction extends LaneValue {
    LaneValue invoke(List<LaneValue> arguments) throws LaneException;
    String getName();

    static LaneEnvironment getBuiltinValues(LaneEnvironment old) throws LaneException {
        var env = new LaneEnvironment.InnerEnvironment(old);
        LaneBuiltinFunction[] functions = {new Add(), new Print()};
        for (var func : functions) {
            addBuiltinFunction(env, func);
        }
        return env;
    }

    static void addBuiltinFunction(LaneEnvironment env, LaneBuiltinFunction function) throws LaneException {
        var name = function.getName();
        var bind = new LaneBinding.ValueBinding(function);
        env.extend(name, bind);
    }

    final class Add implements LaneBuiltinFunction {
        @Override
        public String getName() {
            return "+";
        }

        @Override
        public String print() {
            return "<builtin function: +>";
        }

        @Override
        public LaneValue invoke(List<LaneValue> arguments) throws LaneException {
            if (arguments.size() != 2) {
                throw new LaneBug("The number of operands is not equal to 2 around +: " + arguments);
            }
            if (arguments.get(0) instanceof LaneValue.LaneNumberValue n1 &&
                arguments.get(1) instanceof LaneValue.LaneNumberValue n2) {
                return new LaneValue.LaneNumberValue(n1.content() + n2.content());
            } else {
                throw new LaneException("Types of operands are not <Number> around +: " + arguments.get(0) + " " + arguments.get(1));
            }
        }
    }

    final class Print implements LaneBuiltinFunction {
        @Override
        public String getName() {
            return "print";
        }

        @Override
        public LaneValue invoke(List<LaneValue> arguments) throws LaneException {
            for (var arg : arguments) {
                System.out.println(arg.print());
            }
            return new LaneUnitValue();
        }

        @Override
        public String print() {
            return "<builtin function: print>";
        }
    }
}

