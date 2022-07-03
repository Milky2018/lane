package com.milky.Lane.transformer;

import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;
import com.milky.Lane.type.LaneTypedName;

import java.util.ArrayList;
import java.util.List;

public class TypedParamListTransformer extends LaneBaseVisitor<List<LaneTypedName>> {
    @Override
    public List<LaneTypedName> visitEptTypedParaSpec(LaneParser.EptTypedParaSpecContext ctx) {
        return new ArrayList<>();
    }

    @Override
    public List<LaneTypedName> visitNeptTypedParaSpec(LaneParser.NeptTypedParaSpecContext ctx) {
        var ctxList = ctx.typedPara();
        var list = new ArrayList<LaneTypedName>();
        for (var typedPara : ctxList) {
            var name = typedPara.ID().getText();
            var typeCtx = typedPara.type();
            var type = Transformer.typeTransformer.visit(typeCtx);
            var typedName = new LaneTypedName(name, type);
            list.add(typedName);
        }
        return list;
    }
}
