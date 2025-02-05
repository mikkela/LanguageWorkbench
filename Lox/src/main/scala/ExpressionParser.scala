package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{Parser, ParserResult, Success, Token, LookaheadIterator}

object primaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    matchToken[NumberToken, StringToken, TrueToken, FalseToken, NilToken, LeftParenthesisToken](bufferedTokens) match
      case Some(n: NumberToken) => Success(LiteralNode(n.lexeme.toDouble))
      case Some(s: StringToken) => Success(LiteralNode(s.lexeme))
      case Some(b: TrueToken) => Success(LiteralNode(true))
      case Some(b: FalseToken) => Success(LiteralNode(false))
      case Some(n: NilToken) => Success(LiteralNode(null))
      case Some(n: LeftParenthesisToken) => expressionParser.parse(bufferedTokens).flatMap {
        expression => matchToken[RightParenthesisToken](bufferedTokens) match
            case Some(t) => Success(GroupNode(expression))
            case None => handleUnmatchedToken(bufferedTokens.headOption)
      }
      case _ => handleUnmatchedToken(bufferedTokens.headOption)

object unaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    matchToken[BangToken, MinusToken](bufferedTokens) match
      case Some(t) =>
        primaryParser.parse(tokens).flatMap {
          expression => Success(UnaryNode(t, expression))
        }
      case None => primaryParser.parse(tokens)

object factorParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    unaryParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[SlashToken, StarToken](bufferedTokens) match
          case Some(t) => unaryParser.parse(bufferedTokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object termParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    factorParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[PlusToken, MinusToken](bufferedTokens) match
          case Some(t) => factorParser.parse(bufferedTokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object comparisonParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    termParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[LessToken, LessEqualToken, GreaterToken, GreaterEqualToken](bufferedTokens) match
          case Some(t) => termParser.parse(bufferedTokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object equalityParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    val bufferedTokens = LookaheadIterator[Token](tokens)
    comparisonParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[EqualEqualToken, BangEqualToken](bufferedTokens) match
          case Some(t) => comparisonParser.parse(bufferedTokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object expressionParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): ParserResult[ExpressionNode] =
    equalityParser.parse(tokens)
