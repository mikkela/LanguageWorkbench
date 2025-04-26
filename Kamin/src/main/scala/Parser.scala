package org.mikadocs.language.kamin

import org.mikadocs.language.workbench.{EndOfFileToken, LookaheadIterator, Node, Parser, ParserResult, Success, Token, Unfinished, Value}
import org.mikadocs.language.workbench.IntegerValueExtension

trait ExpressionParser extends Parser[ExpressionNode]

trait ExpressionListParser[TElement <: ExpressionNode, TResult <: ExpressionNode] extends Parser[TResult]:
  def parseList(
             tokens: LookaheadIterator[Token],
             initial: Seq[TElement],
             buildNode: Seq[TElement] => ParserResult[TResult],
             expressionParser: Parser[TElement]
           ): ParserResult[TResult] =
    matchToken[RightParenthesisToken, EndOfFileToken.type](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(EndOfFileToken) => Unfinished
      case None =>
        expressionParser.parse(tokens).flatMap { expr =>
          parseList(tokens, initial.appended(expr), buildNode, expressionParser)
        }

trait ArgumentListParser[T <: Node] extends Parser[T]:
  def parseList(
             tokens: LookaheadIterator[Token],
             initial: Seq[String],
             buildNode: Seq[String] => ParserResult[T]
           ): ParserResult[T] =
    matchToken[RightParenthesisToken, NameToken](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(n: NameToken) => parseList(tokens, initial.appended(n.lexeme), buildNode)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class FunctionDefinitionParser(val expressionParser: ExpressionParser)
  extends Parser[FunctionDefinitionNode], ArgumentListParser[FunctionDefinitionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[FunctionDefinitionNode] =
    (matchToken[LeftParenthesisToken](tokens), matchToken[DefineToken](tokens)) match
      case (Some(_), Some(_)) =>
        matchToken[NameToken](tokens) match
          case Some(function: NameToken) =>
            matchToken[LeftParenthesisToken](tokens) match
              case Some(_) =>
                parseList(
                  tokens,
                  Seq.empty[String],
                  arguments =>
                    expressionParser.parse(tokens).flatMap{
                      expression => Success(FunctionDefinitionNode(function.lexeme, arguments, expression))
                    }

                )

              case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
          case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
      case _ => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class IfExpressionParser(val expressionParser: ExpressionParser) extends Parser[IfExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[IfExpressionNode] =
    // if keyword has been consumed
    expressionParser.parse(tokens).flatMap {
      test => expressionParser.parse(tokens).flatMap {
        consequence => expressionParser.parse(tokens).flatMap {
          alternative =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(IfExpressionNode(test, consequence, alternative))
              case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
        }
      }
    }


class WhileExpressionParser(val expressionParser: ExpressionParser) extends Parser[WhileExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[WhileExpressionNode] =
    // while keyword has been consumed
    expressionParser.parse(tokens).flatMap {
      test =>
        expressionParser.parse(tokens).flatMap {
          body =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(WhileExpressionNode(test, body))
              case None => handleUnmatchedToken(tokens.headOption)

        }
    }

class SetExpressionParser(val expressionParser: ExpressionParser) extends Parser[SetExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[SetExpressionNode] =
    // set keyword has been consumed
    matchToken[NameToken](tokens) match
      case Some(variable: NameToken) =>
        expressionParser.parse(tokens).flatMap {
          value =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(SetExpressionNode(variable.lexeme, value))
              case None => handleUnmatchedToken(tokens.headOption)
        }
      case _ => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class BeginExpressionParser(val expressionParser: ExpressionParser) extends Parser[BeginExpressionNode], ExpressionListParser[ExpressionNode, BeginExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[BeginExpressionNode] =
    // begin keyword has been consumed
    parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(BeginExpressionNode(exprs)), expressionParser)

class OperationExpressionParser(val expressionParser: ExpressionParser) extends Parser[OperationExpressionNode], ExpressionListParser[ExpressionNode, OperationExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[OperationExpressionNode] =
    matchToken[OperatorToken, NameToken](tokens) match
      case Some(op: OperatorToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case Some(op: NameToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case None =>
        handleUnmatchedToken(tokens.headOption)

class SExpressionParser extends Parser[SExpressionNode], ExpressionListParser[SExpressionNode, SExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[SExpressionNode] = {
    matchToken[IntegerToken, NameToken, LeftParenthesisToken](tokens) match {
      case Some(t: IntegerToken) => Success(SExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(t: NameToken) => Success(SExpressionNode(t.lexeme.toSymbolValue))
      case Some(_: LeftParenthesisToken) =>
        parseList(tokens, Seq.empty[SExpressionNode], exprs => Success(SExpressionNode(exprs)), this)
      case None =>
        handleUnmatchedToken(tokens.headOption)
    }
  }
