package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{Parser, ParserResult, Success, Token, LookaheadIterator}

object primaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    matchToken[NumberToken, StringToken, TrueToken, FalseToken, NilToken, LeftParenthesisToken](tokens) match
      case Some(n: NumberToken) => Success(LiteralNode(n.lexeme.toDouble))
      case Some(s: StringToken) => Success(LiteralNode(s.lexeme))
      case Some(b: TrueToken) => Success(LiteralNode(true))
      case Some(b: FalseToken) => Success(LiteralNode(false))
      case Some(n: NilToken) => Success(LiteralNode(null))
      case Some(n: LeftParenthesisToken) => expressionParser.parse(tokens).flatMap {
        expression => matchToken[RightParenthesisToken](tokens) match
            case Some(t) => Success(GroupNode(expression))
            case None => handleUnmatchedToken(tokens.headOption)
      }
      case _ => handleUnmatchedToken(tokens.headOption)

object unaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    matchToken[BangToken, MinusToken](tokens) match
      case Some(t) =>
        primaryParser.parse(tokens).flatMap {
          expression => Success(UnaryNode(t, expression))
        }
      case None => primaryParser.parse(tokens)

object factorParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    unaryParser.parse(tokens).flatMap {
      leftExpression =>
        matchToken[SlashToken, StarToken](tokens) match
          case Some(t) => unaryParser.parse(tokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object termParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    factorParser.parse(tokens).flatMap {
      leftExpression =>
        matchToken[PlusToken, MinusToken](tokens) match
          case Some(t) => factorParser.parse(tokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object comparisonParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    termParser.parse(tokens).flatMap {
      leftExpression =>
        matchToken[LessToken, LessEqualToken, GreaterToken, GreaterEqualToken](tokens) match
          case Some(t) => termParser.parse(tokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object equalityParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    comparisonParser.parse(tokens).flatMap {
      leftExpression =>
        matchToken[EqualEqualToken, BangEqualToken](tokens) match
          case Some(t) => comparisonParser.parse(tokens).flatMap {
              rightExpression => Success(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Success(leftExpression)
    }

object expressionParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    equalityParser.parse(tokens)
