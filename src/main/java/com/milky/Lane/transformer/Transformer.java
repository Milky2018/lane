package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneProgram;
import com.milky.Lane.parser.LaneParser;

public class Transformer {
    static final ProgramTransformer programTransformer = new ProgramTransformer();
    static final ArgListTransformer argListTransformer = new ArgListTransformer();
    static final ExpressionTransformer expressionTransformer = new ExpressionTransformer();
    static final TopLevelStatementTransformer topLevelStatementTransformer = new TopLevelStatementTransformer();
    static final StatementTransformer statementTransformer = new StatementTransformer();
    static final NamedFunctionDefinitionTransformer namedFunctionDefinitionTransformer = new NamedFunctionDefinitionTransformer();
    static final UnnamedFunctionDefinitionTransformer unnamedFunctionDefinitionTransformer = new UnnamedFunctionDefinitionTransformer();
    static final FunctionDefinitionTailTransformer functionDefinitionTailTransformer = new FunctionDefinitionTailTransformer();
    static final TypeTransformer typeTransformer = new TypeTransformer();
    static final ArgsTypeTransformer argsTypeTransformer = new ArgsTypeTransformer();
    static final TypedParamListTransformer typedParamListTransformer = new TypedParamListTransformer();

    public static LaneProgram transform(LaneParser.ProgContext ctx) {
        return programTransformer.visitProg(ctx);
    }
}
