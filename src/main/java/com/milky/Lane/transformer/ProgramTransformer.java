package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneProgram;
import com.milky.Lane.parser.LaneBaseVisitor;
import com.milky.Lane.parser.LaneParser;

import static com.milky.Lane.transformer.Transformer.topLevelStatementTransformer;

public class ProgramTransformer extends LaneBaseVisitor<LaneProgram> {
    @Override
    public LaneProgram visitProg(LaneParser.ProgContext ctx) {
        var listOfTlsContext = ctx.tls();
        var listOfTls = listOfTlsContext.stream().map(topLevelStatementTransformer::visit).toList();
        return new LaneProgram(listOfTls);
    }
}
