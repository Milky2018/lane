package com.milky.Lane.transformer;

import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;
import com.milky.Lane.type.LaneType;

import java.util.ArrayList;
import java.util.List;

public class ArgsTypeTransformer extends LaneBaseVisitor<List<LaneType>> {
    @Override
    public List<LaneType> visitEptArgTypeList(LaneParser.EptArgTypeListContext ctx) {
        return new ArrayList<>();
    }

    @Override
    public List<LaneType> visitNeptArgTypeList(LaneParser.NeptArgTypeListContext ctx) {
        var ctxList = ctx.type();
        return ctxList.stream().map(Transformer.typeTransformer::visit).toList();
    }
}
