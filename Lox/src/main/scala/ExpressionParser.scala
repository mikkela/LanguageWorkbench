package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{Parser, Token, TokenTypeMatcher, TokensMatcher}

object PrimaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    matchToken[NumberToken, StringToken, TrueToken, FalseToken, NilToken, LeftParenthesisToken](bufferedTokens) match
      case Some(n: NumberToken) => Right(LiteralNode(n.lexeme.toDouble))
      case Some(s: StringToken) => Right(LiteralNode(s.lexeme))
      case Some(b: TrueToken) => Right(LiteralNode(true))
      case Some(b: FalseToken) => Right(LiteralNode(false))
      case Some(n: NilToken) => Right(LiteralNode(null))
      case Some(n: LeftParenthesisToken) => ExpressionParser.parse(bufferedTokens).flatMap {
        expression => matchToken[RightParenthesisToken](bufferedTokens) match
            case Some(t) => Right(expression)
            case None => parseError(bufferedTokens.head)
      }
      case _ => parseError(bufferedTokens.head)

object UnaryParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    matchToken[BangToken, MinusToken](bufferedTokens) match
      case Some(t) =>
        PrimaryParser.parse(tokens).flatMap {
          expression => Right(UnaryNode(t, expression))
        }
      case None => PrimaryParser.parse(tokens)

object FactorParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    UnaryParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[SlashToken, StarToken](bufferedTokens) match
          case Some(t) => UnaryParser.parse(bufferedTokens).flatMap {
              rightExpression => Right(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Right(leftExpression)
    }

object TermParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    FactorParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[PlusToken, MinusToken](bufferedTokens) match
          case Some(t) => FactorParser.parse(bufferedTokens).flatMap {
              rightExpression => Right(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Right(leftExpression)
    }

object ComparisonParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    TermParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[LessToken, LessEqualToken, GreaterToken, GreaterEqualToken](bufferedTokens) match
          case Some(t) => TermParser.parse(bufferedTokens).flatMap {
              rightExpression => Right(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Right(leftExpression)
    }

object EqualityParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    val bufferedTokens = tokens.buffered
    ComparisonParser.parse(bufferedTokens).flatMap {
      leftExpression =>
        matchToken[EqualEqualToken, BangEqualToken](bufferedTokens) match
          case Some(t) => ComparisonParser.parse(bufferedTokens).flatMap {
              rightExpression => Right(BinaryNode(t, leftExpression, rightExpression))
            }
          case _ => Right(leftExpression)
    }

object ExpressionParser extends Parser[ExpressionNode]:
  override def parse(tokens: Iterator[Token]): Either[String, ExpressionNode] =
    EqualityParser.parse(tokens)
