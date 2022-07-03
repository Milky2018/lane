package com.milky.Lane.transformer;

import com.milky.Lane.ast.LaneStatement;
import com.milky.Lane.eval.TypedParameterList;
import com.milky.Lane.type.LaneTypedName;

import java.util.List;

public record FuncDefTail(List<LaneTypedName> parameterList, LaneStatement body) {
}
