// Generated from /Users/zhengyu/Documents/语言学习/java/Lane/src/main/antlr/Lane.g4 by ANTLR 4.10.1
package com.milky.Lane.parser;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link LaneParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface LaneVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link LaneParser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg(LaneParser.ProgContext ctx);
	/**
	 * Visit a parse tree produced by the {@code topFuncDef}
	 * labeled alternative in {@link LaneParser#tls}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTopFuncDef(LaneParser.TopFuncDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link LaneParser#namedFuncDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNamedFuncDef(LaneParser.NamedFuncDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link LaneParser#unnamedFuncDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnnamedFuncDef(LaneParser.UnnamedFuncDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link LaneParser#funcDefTail}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFuncDefTail(LaneParser.FuncDefTailContext ctx);
	/**
	 * Visit a parse tree produced by {@link LaneParser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(LaneParser.BlockContext ctx);
	/**
	 * Visit a parse tree produced by the {@code cmpStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmpStat(LaneParser.CmpStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockStat(LaneParser.BlockStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code letMutBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLetMutBindStat(LaneParser.LetMutBindStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code exprStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExprStat(LaneParser.ExprStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code letBindStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLetBindStat(LaneParser.LetBindStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code assignStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignStat(LaneParser.AssignStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code retStat}
	 * labeled alternative in {@link LaneParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRetStat(LaneParser.RetStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code stringExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStringExpr(LaneParser.StringExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code appExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAppExpr(LaneParser.AppExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code funcExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFuncExpr(LaneParser.FuncExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code lambdaExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambdaExpr(LaneParser.LambdaExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code uOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUOpExpr(LaneParser.UOpExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code binOpExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinOpExpr(LaneParser.BinOpExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code nstExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNstExpr(LaneParser.NstExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolExpr(LaneParser.BoolExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumExpr(LaneParser.NumExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link LaneParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdExpr(LaneParser.IdExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code litType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLitType(LaneParser.LitTypeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code funcType}
	 * labeled alternative in {@link LaneParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFuncType(LaneParser.FuncTypeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEptArgTypeList(LaneParser.EptArgTypeListContext ctx);
	/**
	 * Visit a parse tree produced by the {@code neptArgTypeList}
	 * labeled alternative in {@link LaneParser#argsType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNeptArgTypeList(LaneParser.NeptArgTypeListContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEptArgList(LaneParser.EptArgListContext ctx);
	/**
	 * Visit a parse tree produced by the {@code neptArgList}
	 * labeled alternative in {@link LaneParser#argList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNeptArgList(LaneParser.NeptArgListContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEptParaSpec(LaneParser.EptParaSpecContext ctx);
	/**
	 * Visit a parse tree produced by the {@code neptParaSpec}
	 * labeled alternative in {@link LaneParser#paraSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNeptParaSpec(LaneParser.NeptParaSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link LaneParser#typedPara}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypedPara(LaneParser.TypedParaContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEptTypedParaSpec(LaneParser.EptTypedParaSpecContext ctx);
	/**
	 * Visit a parse tree produced by the {@code neptTypedParaSpec}
	 * labeled alternative in {@link LaneParser#typedParaSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNeptTypedParaSpec(LaneParser.NeptTypedParaSpecContext ctx);
}