package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{LookaheadIterator, Node, Parser, ParserResult, Success, Token, prepend, IntegerValueExtension}

object basicParser extends Parser[ExpressionNode | FunctionDefinitionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode | FunctionDefinitionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)

    val lookahead = bufferedTokens.lookahead(3)
    bufferedTokens.lookahead(2) match
      case Seq(t1: LeftParenthesisToken, t2: DefineToken) =>
        functionDefinitionParser.parse(bufferedTokens.asIterator)
      case _ =>
        expressionParser.parse(bufferedTokens.asIterator)


object functionDefinitionParser extends Parser[FunctionDefinitionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[FunctionDefinitionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)

    /*matchToken[LeftParenthesisToken](bufferedTokens) match
      case Some(_) =>
        matchToken[DefineToken](bufferedTokens) match
          case Some(_) =>
            ???
          case _ => handleUnmatchedToken(bufferedTokens.lookahead(1).headOption)
      case _ => handleUnmatchedToken(bufferedTokens.lookahead(1).headOption)*/
    (matchToken[LeftParenthesisToken](bufferedTokens), matchToken[DefineToken](bufferedTokens)) match
      case (Some(_), Some(_)) => ???
      case _ => handleUnmatchedToken(bufferedTokens.lookahead(1).headOption, acceptUnfinished = true)

object expressionParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    matchToken[IntegerToken, NameToken, LeftParenthesisToken](bufferedTokens) match
      case Some(t: IntegerToken) => Success(ValueExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(t: NameToken) => Success(VariableExpressionNode(t))
      case Some(_: LeftParenthesisToken) =>
        matchToken[IfToken, WhileToken, SetToken, BeginToken, OperatorToken, NameToken](bufferedTokens) match
          case Some(_: IfToken) => ifExpressionParser.parse(bufferedTokens.asIterator)
          case Some(_: WhileToken) => whileExpressionParser.parse(bufferedTokens.asIterator)
          case Some(_: SetToken) => setExpressionParser.parse(tokens.asIterator)
          case Some(_: BeginToken) => beginExpressionParser.parse(tokens.asIterator)
          case Some(t: OperatorToken) => operationExpressionParser.parse(prepend(t, tokens.asIterator))
          case Some(t: NameToken) => operationExpressionParser.parse(prepend(t, tokens.asIterator))
          case None => handleUnmatchedToken(bufferedTokens.lookahead(1).headOption, acceptUnfinished = true)
      case None => handleUnmatchedToken(bufferedTokens.lookahead(1).headOption, acceptUnfinished = true)

object ifExpressionParser extends Parser[IfExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[IfExpressionNode] =
    // if keyword has been consumed
    val bufferedTokens = LookaheadIterator[Token](tokens)
    expressionParser.parse(tokens).flatMap {
      test => expressionParser.parse(tokens).flatMap {
        consequence => expressionParser.parse(tokens).flatMap {
          alternative =>
            val bufferedTokens = LookaheadIterator[Token](tokens)
            matchToken[RightParenthesisToken](bufferedTokens) match
              case Some(_) => Success(IfExpressionNode(test, consequence, alternative))
              case None => handleUnmatchedToken(bufferedTokens.headOption, acceptUnfinished = true)
        }
      }
    }


object whileExpressionParser extends Parser[WhileExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[WhileExpressionNode] =
    // while keyword has been consumed
    expressionParser.parse(tokens).flatMap {
      test =>
        expressionParser.parse(tokens).flatMap {
          body =>
            val bufferedTokens = LookaheadIterator[Token](tokens)
            matchToken[RightParenthesisToken](bufferedTokens) match
              case Some(_) => Success(WhileExpressionNode(test, body))
              case None => handleUnmatchedToken(bufferedTokens.headOption)
            
        }
    }

object setExpressionParser extends Parser[SetExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[SetExpressionNode] = ???

object beginExpressionParser extends Parser[BeginExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[BeginExpressionNode] = ???

object operationExpressionParser extends Parser[OperationExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[OperationExpressionNode] = ???

object expressionListParser
