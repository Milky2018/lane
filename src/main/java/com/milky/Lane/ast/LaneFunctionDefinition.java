package com.milky.Lane.ast;

import com.milky.Lane.eval.LaneEnvironment;
import com.milky.Lane.eval.LaneValue;
import com.milky.Lane.eval.TypedParameterList;
import com.milky.Lane.type.LaneType;
import com.milky.Lane.type.LaneTypeEnvironment;

sealed public interface LaneFunctionDefinition {
    String prettyPrint();
    LaneValue evaluate(LaneEnvironment env);
//    LaneType typeCheck(LaneTypeEnvironment env);

    record FunctionWithStatement(TypedParameterList parameterList, LaneStatement body) implements LaneFunctionDefinition {
        @Override
        public String prettyPrint() {
            var text = new StringBuilder();
            for (var param : parameterList.params()) {
                text.append(param);
                text.append(" ");
            }
            text.append(body.prettyPrint());
            return text.toString();
        }

        @Override
        public LaneValue evaluate(LaneEnvironment env) {
            return new LaneValue.LaneFunctionValue(parameterList, body, env);
        }
    }

    record FunctionWithExpression(TypedParameterList parameterList, LaneExpression body) implements LaneFunctionDefinition {
        @Override
        public String prettyPrint() {
            var text = new StringBuilder();
            for (var param : parameterList.params()) {
                text.append(param);
                text.append(" ");
            }
            text.append("-> ");
            text.append(body.prettyPrint());
            return text.toString();
        }

        @Override
        public LaneValue evaluate(LaneEnvironment env) {
            var stat = new LaneStatement.LaneReturnStatement(body);
            return new LaneValue.LaneFunctionValue(parameterList, stat, env);
        }
    }
}
