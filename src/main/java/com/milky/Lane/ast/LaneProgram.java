package com.milky.Lane.ast;

import com.milky.Lane.builtin.LaneBuiltinFunction;
import com.milky.Lane.eval.*;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.exception.LaneTypeException;
import com.milky.Lane.type.LaneTypeEnvironment;

import java.util.ArrayList;
import java.util.List;

public record LaneProgram(List<LaneTopLevelStatement> listOfTls) {
    public String prettyPrint() {
        var text = new StringBuilder();
        text.append("Lane Program\n");
        for (var tls : listOfTls) {
            text.append(tls.prettyPrint());
            text.append("\n");
        }
        return text.toString();
    }

    public LaneEnvironment scan(LaneEnvironment env) throws LaneException {
        var topLevelEnv = new LaneEnvironment.InnerEnvironment(env);
        for (var tls : this.listOfTls) {
            if (tls instanceof LaneTopLevelStatement.LaneNamedFunctionDefinition named) {
                var name = named.functionName();
                var funcDef = named.functionDefinition();
                var bind = new LaneBinding.FunctionBinding(funcDef);
                topLevelEnv.extend(name, bind);
            }
        }
        return topLevelEnv;
    }

    public LaneFinalResult evaluateK() throws LaneException {
        var empty = new LaneEnvironment.EmptyEnvironment();
        var builtin = LaneBuiltinFunction.getBuiltinValues(empty);
        var env = this.scan(builtin);
        var mainFunc = env.lookup("main");
        var cont = new LaneContinuation.EndContinuation();
        var falseArguments = new ArrayList<LaneValue>();
        if (mainFunc instanceof LaneValue.LaneFunctionValue function) {
            return function.invokeK(falseArguments, cont);
        } else {
            throw new LaneException("main is not a function");
        }
    }

    public LaneTypeEnvironment typeScan(LaneTypeEnvironment env) throws LaneTypeException {
        throw new LaneTypeException("Not implemented!");
    }

    public void typeCheck() {
        // TODO
    }
}
