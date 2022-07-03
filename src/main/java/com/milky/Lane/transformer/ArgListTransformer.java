package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneExpression;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

import java.util.ArrayList;
import java.util.List;

import static com.milky.Lane.transformer.Transformer.expressionTransformer;

public class ArgListTransformer extends LaneBaseVisitor<List<LaneExpression>> {
    @Override
    public List<LaneExpression> visitEptArgList(LaneParser.EptArgListContext ctx) {
        return new ArrayList<>();
    }

    @Override
    public List<LaneExpression> visitNeptArgList(LaneParser.NeptArgListContext ctx) {
        var ctxList = ctx.expr();
        return ctxList.stream().map(expressionTransformer::visit).toList();
    }
}
