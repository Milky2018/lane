package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneExpression;
import com.milky.Lane.ast.LaneFunctionDefinition;
import com.milky.Lane.eval.TypedParameterList;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

import java.util.List;

import static com.milky.Lane.transformer.Transformer.*;

public class ExpressionTransformer extends LaneBaseVisitor<LaneExpression> {
    @Override
    public LaneExpression visitNumExpr(LaneParser.NumExprContext ctx) {
        var numString = ctx.NUM().getText();
        var num = Integer.parseInt(numString);
        return new LaneExpression.LaneNumExpression(num);
    }

    @Override
    public LaneExpression visitIdExpr(LaneParser.IdExprContext ctx) {
        var id = ctx.ID().getText();
        return new LaneExpression.LaneIdExpression(id);
    }

    @Override
    public LaneExpression visitBoolExpr(LaneParser.BoolExprContext ctx) {
        var id = ctx.BOOL().getText();
        if (id.equals("true")) {
            return new LaneExpression.LaneBoolExpression(true);
        } else {
            return new LaneExpression.LaneBoolExpression(false);
        }
    }

    @Override
    public LaneExpression visitStringExpr(LaneParser.StringExprContext ctx) {
        var str = ctx.STRING().getText();
        return new LaneExpression.LaneStringExpression(str.substring(1, str.length() - 1));
    }

    @Override
    public LaneExpression visitUOpExpr(LaneParser.UOpExprContext ctx) {
        var op = ctx.op.getText();
        var opFunc = new LaneExpression.LaneIdExpression(op);
        var operand = ctx.expr();
        var arg = expressionTransformer.visit(operand);
        return new LaneExpression.LaneApplicationExpression(opFunc, List.of(arg));
    }

    @Override
    public LaneExpression visitBinOpExpr(LaneParser.BinOpExprContext ctx) {
        var op = ctx.op.getText();
        var opFunc = new LaneExpression.LaneIdExpression(op);
        var operand1 = ctx.a;
        var arg1 = expressionTransformer.visit(operand1);
        var operand2 = ctx.b;
        var arg2 = expressionTransformer.visit(operand2);
        return new LaneExpression.LaneApplicationExpression(opFunc, List.of(arg1, arg2));
    }

    @Override
    public LaneExpression visitLambdaExpr(LaneParser.LambdaExprContext ctx) {
        var paramCtx = ctx.typedParaSpec();
        var paramList = typedParamListTransformer.visit(paramCtx);
        var params = new TypedParameterList(paramList);
        var bodyCtx = ctx.expr();
        var body = expressionTransformer.visit(bodyCtx);
        var def =  new LaneFunctionDefinition.FunctionWithExpression(params, body);
        return new LaneExpression.LaneFunctionExpression(def);
    }

    @Override
    public LaneExpression visitFuncExpr(LaneParser.FuncExprContext ctx) {
        var funcDefCtx = ctx.unnamedFuncDef();
        var def = unnamedFunctionDefinitionTransformer.visit(funcDefCtx);
        return new LaneExpression.LaneFunctionExpression(def);
    }

    @Override
    public LaneExpression visitAppExpr(LaneParser.AppExprContext ctx) {
        var func = this.visit(ctx.expr());
        var argCtxList = ctx.argList();
        var argList = argListTransformer.visit(argCtxList);
        return new LaneExpression.LaneApplicationExpression(func, argList);
    }

    @Override
    public LaneExpression visitNstExpr(LaneParser.NstExprContext ctx) {
        return this.visit(ctx.expr());
    }
}
