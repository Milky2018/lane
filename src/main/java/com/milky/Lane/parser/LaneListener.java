// Generated from /Users/zhengyu/Documents/语言学习/java/Lane/src/main/antlr/Lane.g4 by ANTLR 4.10.1
package com.milky.Lane.parser;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link LaneParser}.
 */
public interface LaneListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link LaneParser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(LaneParser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(LaneParser.ProgContext ctx);
	/**
	 * Enter a parse tree produced by the {@code topFuncDef}
	 * labeled alternative in {@link LaneParser#tls}.
	 * @param ctx the parse tree
	 */
	void enterTopFuncDef(LaneParser.TopFuncDefContext ctx);
	/**
	 * Exit a parse tree produced by the {@code topFuncDef}
	 * labeled alternative in {@link LaneParser#tls}.
	 * @param ctx the parse tree
	 */
	void exitTopFuncDef(LaneParser.TopFuncDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link LaneParser#namedFuncDef}.
	 * @param ctx the parse tree
	 */
	void enterNamedFuncDef(LaneParser.NamedFuncDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#namedFuncDef}.
	 * @param ctx the parse tree
	 */
	void exitNamedFuncDef(LaneParser.NamedFuncDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link LaneParser#unnamedFuncDef}.
	 * @param ctx the parse tree
	 */
	void enterUnnamedFuncDef(LaneParser.UnnamedFuncDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#unnamedFuncDef}.
	 * @param ctx the parse tree
	 */
	void exitUnnamedFuncDef(LaneParser.UnnamedFuncDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link LaneParser#funcDefTail}.
	 * @param ctx the parse tree
	 */
	void enterFuncDefTail(LaneParser.FuncDefTailContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#funcDefTail}.
	 * @param ctx the parse tree
	 */
	void exitFuncDefTail(LaneParser.FuncDefTailContext ctx);
	/**
	 * Enter a parse tree produced by {@link LaneParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(LaneParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(LaneParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by the {@code cmpStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterCmpStat(LaneParser.CmpStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code cmpStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitCmpStat(LaneParser.CmpStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterBlockStat(LaneParser.BlockStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitBlockStat(LaneParser.BlockStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code letMutBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterLetMutBindStat(LaneParser.LetMutBindStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code letMutBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitLetMutBindStat(LaneParser.LetMutBindStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code exprStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterExprStat(LaneParser.ExprStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code exprStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitExprStat(LaneParser.ExprStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code letBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterLetBindStat(LaneParser.LetBindStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code letBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitLetBindStat(LaneParser.LetBindStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code assignStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterAssignStat(LaneParser.AssignStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code assignStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitAssignStat(LaneParser.AssignStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code retStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterRetStat(LaneParser.RetStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code retStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitRetStat(LaneParser.RetStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code stringExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterStringExpr(LaneParser.StringExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code stringExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitStringExpr(LaneParser.StringExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code appExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterAppExpr(LaneParser.AppExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code appExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitAppExpr(LaneParser.AppExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code funcExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterFuncExpr(LaneParser.FuncExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code funcExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitFuncExpr(LaneParser.FuncExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code lambdaExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterLambdaExpr(LaneParser.LambdaExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code lambdaExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitLambdaExpr(LaneParser.LambdaExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code uOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterUOpExpr(LaneParser.UOpExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code uOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitUOpExpr(LaneParser.UOpExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code binOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterBinOpExpr(LaneParser.BinOpExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code binOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitBinOpExpr(LaneParser.BinOpExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code nstExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterNstExpr(LaneParser.NstExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code nstExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitNstExpr(LaneParser.NstExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterBoolExpr(LaneParser.BoolExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitBoolExpr(LaneParser.BoolExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterNumExpr(LaneParser.NumExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitNumExpr(LaneParser.NumExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterIdExpr(LaneParser.IdExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitIdExpr(LaneParser.IdExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code litType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 */
	void enterLitType(LaneParser.LitTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code litType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 */
	void exitLitType(LaneParser.LitTypeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code funcType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 */
	void enterFuncType(LaneParser.FuncTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code funcType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 */
	void exitFuncType(LaneParser.FuncTypeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 */
	void enterEptArgTypeList(LaneParser.EptArgTypeListContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 */
	void exitEptArgTypeList(LaneParser.EptArgTypeListContext ctx);
	/**
	 * Enter a parse tree produced by the {@code neptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 */
	void enterNeptArgTypeList(LaneParser.NeptArgTypeListContext ctx);
	/**
	 * Exit a parse tree produced by the {@code neptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 */
	void exitNeptArgTypeList(LaneParser.NeptArgTypeListContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 */
	void enterEptArgList(LaneParser.EptArgListContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 */
	void exitEptArgList(LaneParser.EptArgListContext ctx);
	/**
	 * Enter a parse tree produced by the {@code neptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 */
	void enterNeptArgList(LaneParser.NeptArgListContext ctx);
	/**
	 * Exit a parse tree produced by the {@code neptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 */
	void exitNeptArgList(LaneParser.NeptArgListContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 */
	void enterEptParaSpec(LaneParser.EptParaSpecContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 */
	void exitEptParaSpec(LaneParser.EptParaSpecContext ctx);
	/**
	 * Enter a parse tree produced by the {@code neptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 */
	void enterNeptParaSpec(LaneParser.NeptParaSpecContext ctx);
	/**
	 * Exit a parse tree produced by the {@code neptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 */
	void exitNeptParaSpec(LaneParser.NeptParaSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link LaneParser#typedPara}.
	 * @param ctx the parse tree
	 */
	void enterTypedPara(LaneParser.TypedParaContext ctx);
	/**
	 * Exit a parse tree produced by {@link LaneParser#typedPara}.
	 * @param ctx the parse tree
	 */
	void exitTypedPara(LaneParser.TypedParaContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 */
	void enterEptTypedParaSpec(LaneParser.EptTypedParaSpecContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 */
	void exitEptTypedParaSpec(LaneParser.EptTypedParaSpecContext ctx);
	/**
	 * Enter a parse tree produced by the {@code neptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 */
	void enterNeptTypedParaSpec(LaneParser.NeptTypedParaSpecContext ctx);
	/**
	 * Exit a parse tree produced by the {@code neptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 */
	void exitNeptTypedParaSpec(LaneParser.NeptTypedParaSpecContext ctx);
}