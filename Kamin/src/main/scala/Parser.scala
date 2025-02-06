package org.mikadocs.language.kamin

import org.mikadocs.language.workbench.{EndOfFileToken, LookaheadIterator, Node, Parser, ParserResult, Success, Token, Unfinished}

trait ExpressionParser extends Parser[ExpressionNode]

trait ExpressionListParser[T <: ExpressionNode] extends Parser[T]:
  def parseList(
             tokens: LookaheadIterator[Token],
             initial: Seq[ExpressionNode],
             buildNode: Seq[ExpressionNode] => ParserResult[T],
             expressionParser: ExpressionParser
           ): ParserResult[T] =
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
             initial: Seq[NameToken],
             buildNode: Seq[NameToken] => ParserResult[T]
           ): ParserResult[T] =
    matchToken[RightParenthesisToken, NameToken](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(n: NameToken) => parseList(tokens, initial.appended(n), buildNode)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
