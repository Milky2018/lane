package com.milky.Lane.transformer;

import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

public class FunctionDefinitionTailTransformer extends LaneBaseVisitor<FuncDefTail> {
    @Override
    public FuncDefTail visitFuncDefTail(LaneParser.FuncDefTailContext ctx) {
        var funcParamSpecCtx = ctx.typedParaSpec();
        var funcParamSpec = Transformer.typedParamListTransformer.visit(funcParamSpecCtx);
        var statCtx = ctx.block().stat();
        var stat = Transformer.statementTransformer.visit(statCtx);
        return new FuncDefTail(funcParamSpec, stat);
    }
}
