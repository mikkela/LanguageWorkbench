package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{IntegerValueExtension, LookaheadIterator, Parser, ParserResult, Success, Token, prepend}

object basicParser extends Parser[ExpressionNode | FunctionDefinitionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode | FunctionDefinitionNode] =
    tokens.lookahead(2) match
      case Seq(t1: LeftParenthesisToken, t2: DefineToken) =>
        functionDefinitionParser.parse(tokens)
      case _ =>
        expressionParser.parse(tokens)


object functionDefinitionParser extends Parser[FunctionDefinitionNode], ArgumentListParser[FunctionDefinitionNode]:
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

object expressionParser extends ExpressionParser:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    matchToken[IntegerToken, NameToken, LeftParenthesisToken](tokens) match
      case Some(t: IntegerToken) => Success(ValueExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(t: NameToken) => Success(VariableExpressionNode(t.lexeme))
      case Some(_: LeftParenthesisToken) =>
        matchToken[IfToken, WhileToken, SetToken, BeginToken, OperatorToken, NameToken](tokens) match
          case Some(_: IfToken) => ifExpressionParser.parse(tokens)
          case Some(_: WhileToken) => whileExpressionParser.parse(tokens)
          case Some(_: SetToken) => setExpressionParser.parse(tokens)
          case Some(_: BeginToken) => beginExpressionParser.parse(tokens)
          case Some(t: OperatorToken) => operationExpressionParser.parse(prepend(t, tokens))
          case Some(t: NameToken) => operationExpressionParser.parse(prepend(t, tokens))
          case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

object ifExpressionParser extends Parser[IfExpressionNode]:
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


object whileExpressionParser extends Parser[WhileExpressionNode]:
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

object setExpressionParser extends Parser[SetExpressionNode]:
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

object beginExpressionParser extends Parser[BeginExpressionNode], ExpressionListParser[BeginExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[BeginExpressionNode] =
    // begin keyword has been consumed
    parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(BeginExpressionNode(exprs)), expressionParser)

object operationExpressionParser extends Parser[OperationExpressionNode], ExpressionListParser[OperationExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[OperationExpressionNode] =
    matchToken[OperatorToken, NameToken](tokens) match
      case Some(op: OperatorToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case Some(op: NameToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case None =>
        handleUnmatchedToken(tokens.headOption)
