package com.milky.Lane.ast;

import com.milky.Lane.eval.*;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.exception.LaneTypeException;
import com.milky.Lane.type.LaneType;
import com.milky.Lane.type.LaneTypeEnvironment;

import java.util.List;

public sealed interface LaneExpression {
    String prettyPrint();
    LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException;
//    LaneType typeCheck(LaneTypeEnvironment env) throws LaneTypeException;

    record LaneIdExpression(String content) implements LaneExpression {
        @Override
        public String prettyPrint() {
            return this.content;
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var val = env.lookup(content);
            return cont.continues(val);
        }

        public LaneType typeCheck(LaneTypeEnvironment env) throws LaneTypeException {
            throw new LaneTypeException("Not implemented!");
        }
    }

    record LaneNumExpression(int content) implements LaneExpression {
        @Override
        public String prettyPrint() {
            return Integer.toString(this.content);
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var val = new LaneValue.LaneNumberValue(content);
            return cont.continues(val);
        }
    }

    record LaneStringExpression(String content) implements LaneExpression {
        @Override
        public String prettyPrint() {
            return this.content;
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var val = new LaneValue.LaneStringValue(content);
            return cont.continues(val);
        }
    }

    record LaneBoolExpression(boolean content) implements LaneExpression {
        @Override
        public String prettyPrint() {
            if (this.content) {
                return "true";
            } else {
                return "false";
            }
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var val = new LaneValue.LaneBoolValue(content);
            return cont.continues(val);
        }
    }

    record LaneApplicationExpression(LaneExpression function, List<LaneExpression> arguments) implements LaneExpression {
        @Override
        public String prettyPrint() {
            return "<function application>";
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var invokerCont = new LaneContinuation.InvokerContinuation(arguments, env, cont);
            return function.evaluateK(env, invokerCont);
        }
    }

    record LaneFunctionExpression(LaneFunctionDefinition definition) implements LaneExpression {
        @Override
        public String prettyPrint() {
            return "anonymous function" + this.definition.prettyPrint();
        }

        @Override
        public LaneFinalResult evaluateK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            return cont.continues(definition.evaluate(env));
        }
    }
}
