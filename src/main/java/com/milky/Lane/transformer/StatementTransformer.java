package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneStatement;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

import static com.milky.Lane.transformer.Transformer.expressionTransformer;

public class StatementTransformer extends LaneBaseVisitor<LaneStatement> {
    @Override
    public LaneStatement visitCmpStat(LaneParser.CmpStatContext ctx) {
        var headCtx = ctx.head;
        var restCtx = ctx.rest;
        var head = this.visit(headCtx);
        var rest = this.visit(restCtx);
        return new LaneStatement.LaneCompositeStatement(head, rest);
    }

    @Override
    public LaneStatement visitLetBindStat(LaneParser.LetBindStatContext ctx) {
        var name = ctx.ID().getText();
        var bodyCtx = ctx.expr();
        var body = expressionTransformer.visit(bodyCtx);
        return new LaneStatement.LaneLetBindStatement(name, body);
    }

    @Override
    public LaneStatement visitLetMutBindStat(LaneParser.LetMutBindStatContext ctx) {
        var name = ctx.ID().getText();
        var bodyCtx = ctx.expr();
        var body = expressionTransformer.visit(bodyCtx);
        return new LaneStatement.LaneLetMutBindStatement(name, body);
    }

    @Override
    public LaneStatement visitAssignStat(LaneParser.AssignStatContext ctx) {
        var name = ctx.ID().getText();
        var bodyCtx = ctx.expr();
        var body = expressionTransformer.visit(bodyCtx);
        return new LaneStatement.LaneAssignStatement(name, body);
    }

    @Override
    public LaneStatement visitRetStat(LaneParser.RetStatContext ctx) {
        var body = expressionTransformer.visit(ctx.expr());
        return new LaneStatement.LaneReturnStatement(body);
    }

    @Override
    public LaneStatement visitExprStat(LaneParser.ExprStatContext ctx) {
        var expr = expressionTransformer.visit(ctx.expr());
        return new LaneStatement.LaneExpressionStatement(expr);
    }

    @Override
    public LaneStatement visitBlockStat(LaneParser.BlockStatContext ctx) {
        var stat = ctx.block().stat();
        return new LaneStatement.LaneBlockStatement(this.visit(stat));
    }
}

