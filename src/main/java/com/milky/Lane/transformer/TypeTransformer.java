package com.milky.Lane.transformer;

import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;
import com.milky.Lane.type.LaneType;

public class TypeTransformer extends LaneBaseVisitor<LaneType> {
    @Override
    public LaneType visitLitType(LaneParser.LitTypeContext ctx) {
        return switch (ctx.TYPELIT().getText()) {
            case "String" -> new LaneType.StringType();
            case "Number" -> new LaneType.NumberType();
            case "Bool" -> new LaneType.BoolType();
            default -> new LaneType.UnitType();
        };
    }

    @Override
    public LaneType visitFuncType(LaneParser.FuncTypeContext ctx) {
        var argsTypeCtx = ctx.argsType();
        var argsType = Transformer.argsTypeTransformer.visit(argsTypeCtx);
        var retTypeCtx = ctx.type();
        var retType = Transformer.typeTransformer.visit(retTypeCtx);
        return new LaneType.FunctionType(argsType, retType);
    }
}
