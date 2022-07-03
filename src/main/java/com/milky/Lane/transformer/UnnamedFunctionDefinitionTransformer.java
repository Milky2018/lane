package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneFunctionDefinition;
import com.milky.Lane.eval.TypedParameterList;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

public class UnnamedFunctionDefinitionTransformer extends LaneBaseVisitor<LaneFunctionDefinition> {
    @Override
    public LaneFunctionDefinition visitUnnamedFuncDef(LaneParser.UnnamedFuncDefContext ctx) {
        var funcTailCtx = ctx.funcDefTail();
        var funcTail = Transformer.functionDefinitionTailTransformer.visit(funcTailCtx);
        var params = new TypedParameterList(funcTail.parameterList());
        return new LaneFunctionDefinition.FunctionWithStatement(params, funcTail.body());
    }
}
