package com.milky.Lane.eval;

import com.milky.Lane.ast.LaneFunctionDefinition;

sealed public interface LaneBinding {
    record ValueBinding(LaneValue value) implements LaneBinding {

    }
    record FunctionBinding(LaneFunctionDefinition definition) implements LaneBinding {

    }
}
