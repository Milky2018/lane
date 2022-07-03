package com.milky.Lane;

import com.milky.Lane.ast.LaneProgram;
import com.milky.Lane.transformer.Transformer;
import com.milky.Lane.parser.LaneLexer;
import com.milky.Lane.parser.LaneParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

public class Lane {
    static LaneProgram parseLaneProgram(String prog) {
        var lexer = new LaneLexer(CharStreams.fromString(prog));
        var tokens = new CommonTokenStream(lexer);
        var parser = new LaneParser(tokens);
        var tree = parser.prog();
        return Transformer.transform(tree);
    }
}
