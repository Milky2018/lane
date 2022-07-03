package com.milky.Lane.ast;

import com.milky.Lane.eval.*;
import com.milky.Lane.exception.LaneException;
import com.milky.Lane.type.LaneType;

public sealed interface LaneStatement {
    String prettyPrint();
    LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException;

    record LaneLetBindStatement(String name, LaneExpression body) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return "let " + name + " = " + body.prettyPrint() + ";\n";
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var letCont = new LaneContinuation.LetBindStatementContinuation(name, env, cont);
            return body.evaluateK(env, letCont);
        }
    }

    record LaneLetMutBindStatement(String name, LaneExpression body) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return "let mut " + name + " = " + body.prettyPrint() + ";\n";
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var letMutCont = new LaneContinuation.LetMutBindStatementContinuation(name, env, cont);
            return body.evaluateK(env, letMutCont);
        }
    }

    record LaneAssignStatement(String name, LaneExpression target) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return name + " = " + target.prettyPrint() + ";\n";
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var assignCont = new LaneContinuation.AssignStatementContinuation(name, env, cont);
            return target.evaluateK(env, assignCont);
        }
    }

    record LaneReturnStatement(LaneExpression body) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return "return " + body.prettyPrint() + ";\n";
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var retCont = new LaneContinuation.ReturnStatementContinuation(cont);
            return body.evaluateK(env, retCont);
        }
    }

    record LaneCompositeStatement(LaneStatement head, LaneStatement rest) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return head.prettyPrint() + "\n" + rest.prettyPrint();
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var newCont = new LaneContinuation.NextStatementContinuation(rest, env, cont);
            return head.executeK(env, newCont);
        }
    }

    record LaneExpressionStatement(LaneExpression content) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return this.content.prettyPrint();
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var exprStatCont = new LaneContinuation.ExpressionStatementContinuation(cont);
            return content.evaluateK(env, exprStatCont);
        }
    }

    record LaneBlockStatement(LaneStatement statement) implements LaneStatement {
        @Override
        public String prettyPrint() {
            return "{\n" + statement.prettyPrint() + "\n}";
        }

        @Override
        public LaneFinalResult executeK(LaneEnvironment env, LaneContinuation cont) throws LaneException {
            var innerEnv = new LaneEnvironment.InnerEnvironment(env);
            return statement.executeK(innerEnv, cont);
        }
    }
}
