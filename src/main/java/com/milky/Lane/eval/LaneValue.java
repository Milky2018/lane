package com.milky.Lane.eval;

import com.milky.Lane.exception.LaneException;
import com.milky.Lane.ast.LaneStatement;

import java.util.List;

public interface LaneValue {
    String print();

    record LaneBoolValue(boolean content) implements LaneValue {
        @Override
        public String print() {
            return "" + content;
        }
    }

    record LaneUnitValue() implements LaneValue {
        @Override
        public String print() {
            return "unit";
        }
    }

    record LaneNumberValue(int content) implements LaneValue {
        @Override
        public String print() {
            return "" + content;
        }
    }

    record LaneStringValue(String content) implements LaneValue {
        @Override
        public String print() {
            return content;
        }
    }

    record LaneFunctionValue(TypedParameterList params, LaneStatement body,
                             LaneEnvironment closureEnv) implements LaneValue {
//        public LaneType getType() {
//            var retType = body.getType();
//            var paramsType = params.params().stream().map(LaneTypedName::type).toList();
//            return new LaneType.FunctionType(paramsType, retType);
//        }

        public LaneFinalResult invokeK(List<LaneValue> arguments, LaneContinuation cont) throws LaneException {
            var paramList = params.params();
            var paramSize = paramList.size();
            var argSize = arguments.size();
            if (paramSize != argSize) {
                var text = new StringBuilder("Function call failed: parameters are: ");
                for (var param : paramList) {
                    text.append(param);
                    text.append(" ");
                }
                text.append("\nBut the arguments' length is: ");
                text.append(argSize);
                throw new LaneException(text.toString());
            }

            var funcEnv = new LaneEnvironment.InnerEnvironment(closureEnv);
            for (int i = 0; i < paramSize; i++) {
                var param = paramList.get(i);
                var arg = arguments.get(i);
                var argBind = new LaneBinding.ValueBinding(arg);
                funcEnv.extend(param.name(), argBind);
            }
            var funcCont = new LaneContinuation.FunctionContinuation(cont);
            return body.executeK(funcEnv, funcCont);
        }

        @Override
        public String print() {
            return "<function>";
        }
    }

}