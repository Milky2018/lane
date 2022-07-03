package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneFunctionDefinition;
import com.milky.Lane.ast.LaneTopLevelStatement;
import com.milky.Lane.eval.TypedParameterList;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

public class NamedFunctionDefinitionTransformer extends LaneBaseVisitor<LaneTopLevelStatement> {
    @Override
    public LaneTopLevelStatement visitNamedFuncDef(LaneParser.NamedFuncDefContext ctx) {
        var funcName = ctx.ID().getText();
        var funcTailCtx = ctx.funcDefTail();
        var funcTail = Transformer.functionDefinitionTailTransformer.visit(funcTailCtx);
        var params = new TypedParameterList(funcTail.parameterList());
        var funcDef = new LaneFunctionDefinition.FunctionWithStatement(params, funcTail.body());
        return new LaneTopLevelStatement.LaneNamedFunctionDefinition(funcName, funcDef);
    }
}
