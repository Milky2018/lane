package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneTopLevelStatement;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

import static com.milky.Lane.transformer.Transformer.*;

public class TopLevelStatementTransformer extends LaneBaseVisitor<LaneTopLevelStatement> {
    @Override
    public LaneTopLevelStatement visitTopFuncDef(LaneParser.TopFuncDefContext ctx) {
        var namedFuncDef = ctx.namedFuncDef();
        return namedFunctionDefinitionTransformer.visit(namedFuncDef);
    }
    //    public LaneTopLevelStatement.LaneNamedFunctionDefinition visitFuncDef(LaneParser.FuncDefContext ctx) {
//        var funcName = ctx.ID().getText();
//        var funcParamSpecCtx = ctx.paraSpec();
//        var statCtx = ctx.stats();
//        var statList = statementsTransformer.visit(statCtx);
//        var funcParamList = paramListTransformer.visit(funcParamSpecCtx);
//
//        var funcBody = new LaneStatement.LaneCompositeStatement(statList);
//        var funcDef = new LaneFunctionDefinition.FunctionWithStatement(funcParamList, funcBody);
//        return new LaneTopLevelStatement.LaneNamedFunctionDefinition(funcName, funcDef);
//    }
}
