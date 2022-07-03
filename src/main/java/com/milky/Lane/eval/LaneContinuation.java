package com.milky.Lane.eval;

import com.milky.Lane.ast.LaneExpression;
import com.milky.Lane.ast.LaneStatement;
import com.milky.Lane.builtin.LaneBuiltinFunction;
import com.milky.Lane.exception.LaneBug;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.utils.ImmutableList;

import java.util.List;

public interface LaneContinuation {
    LaneFinalResult continues(LaneValue value) throws LaneException;

    LaneFinalResult returns(LaneValue value) throws LaneException;

    record EndContinuation() implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            return new LaneFinalResult(value);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            throw new LaneException("Function no returns");
        }
    }

    record NextStatementContinuation(LaneStatement next, LaneEnvironment env,
                                     LaneContinuation continuation) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            return next.executeK(env, continuation);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return continuation.returns(value);
        }
    }

    record ReturnStatementContinuation(LaneContinuation savedCont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            return savedCont.returns(value);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            throw new LaneBug("Returns after an return: " + savedCont + " value: " + value);
        }
    }

    record LetBindStatementContinuation(String var, LaneEnvironment env,
                                        LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            var bind = new LaneBinding.ValueBinding(value);
            env.extend(var, bind);
            return cont.continues(new LaneValue.LaneUnitValue());
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }

    record LetMutBindStatementContinuation(String var, LaneEnvironment env,
                                           LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            var bind = new LaneBinding.ValueBinding(value);
            env.extend(var, bind);
            return cont.continues(new LaneValue.LaneUnitValue());
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }

    record AssignStatementContinuation(String var, LaneEnvironment env,
                                       LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            env.mutate(var, value);
            return cont.continues(new LaneValue.LaneUnitValue());
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }

    record ExpressionStatementContinuation(LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            return cont.continues(new LaneValue.LaneUnitValue());
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }

    record ArgumentAnalysisContinuation(LaneValue function, List<LaneExpression> arguments,
                                        ImmutableList<LaneValue> evaluated, int index, LaneEnvironment env,
                                        LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            var newEvaluated = new ImmutableList.Cons<>(value, evaluated);
            return invokerAndInvoked(function, arguments, newEvaluated, index, env, cont);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }

    record InvokerContinuation(List<LaneExpression> arguments, LaneEnvironment env,
                               LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            var emptyValues = new ImmutableList.Empty<LaneValue>();
            return invokerAndInvoked(value, arguments, emptyValues, 0, env, cont);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.returns(value);
        }
    }


    static LaneFinalResult invokerAndInvoked(LaneValue function,
                                             List<LaneExpression> arguments,
                                             ImmutableList<LaneValue> evaluated,
                                             int index,
                                             LaneEnvironment env,
                                             LaneContinuation cont) throws LaneException {
        var size = arguments.size();
        if (size == index) {
            var argList = evaluated.toList();
            return switch (function) {
                case LaneValue.LaneFunctionValue laneFunction -> laneFunction.invokeK(argList, cont);
                case LaneBuiltinFunction builtinFunction -> cont.continues(builtinFunction.invoke(argList));
                default -> throw new LaneException("Application's function is not a function: " + function);
            };
        } else {
            var argExpr = arguments.get(index);
            var argCont = new ArgumentAnalysisContinuation(function, arguments, evaluated, index + 1, env, cont);
            return argExpr.evaluateK(env, argCont);
        }
    }

    record FunctionContinuation(LaneContinuation cont) implements LaneContinuation {
        @Override
        public LaneFinalResult continues(LaneValue value) throws LaneException {
            return cont.continues(value);
        }

        @Override
        public LaneFinalResult returns(LaneValue value) throws LaneException {
            return cont.continues(value);
        }
    }
}
