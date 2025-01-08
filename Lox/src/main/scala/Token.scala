package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{SourcePosition, Token}

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
case class NumberToken(lexeme: String, position: SourcePosition) extends Token
abstract class PunctuationToken(val lexeme: String, val position: SourcePosition) extends Token

case class DotToken(override val position: SourcePosition) extends PunctuationToken(DotToken.dot, position)
object DotToken:
  final val dot = "."
case class LeftParenthesisToken(override val position: SourcePosition) extends PunctuationToken(LeftParenthesisToken.leftParenthesis, position)
object LeftParenthesisToken:
  final val leftParenthesis = "("
case class RightParenthesisToken(override val position: SourcePosition) extends PunctuationToken(RightParenthesisToken.rightParenthesis, position)
object RightParenthesisToken:
  final val rightParenthesis = ")"
case class LeftBraceToken(override val position: SourcePosition) extends PunctuationToken(LeftBraceToken.leftBrace, position)
object LeftBraceToken:
  final val leftBrace = "{"
case class RightBraceToken(override val position: SourcePosition) extends PunctuationToken(RightBraceToken.rightBrace, position)
object RightBraceToken:
  final val rightBrace = "}"
case class SemicolonToken(override val position: SourcePosition) extends PunctuationToken(SemicolonToken.semicolon, position)
object SemicolonToken:
  final val semicolon = ";"
case class CommaToken(override val position: SourcePosition) extends PunctuationToken(CommaToken.comma, position)
object CommaToken:
  final val comma = ","
case class PlusToken(override val position: SourcePosition) extends PunctuationToken(PlusToken.plus, position)
object PlusToken:
  final val plus = "+"
case class MinusToken(override val position: SourcePosition) extends PunctuationToken(MinusToken.minus, position)
object MinusToken:
  final val minus = "-"
case class StarToken(override val position: SourcePosition) extends PunctuationToken(StarToken.star, position)
object StarToken:
  final val star = "*"
case class BangEqualToken(override val position: SourcePosition) extends PunctuationToken(BangEqualToken.bangEqual, position)
object BangEqualToken:
  final val bangEqual = "!="
case class EqualEqualToken(override val position: SourcePosition) extends PunctuationToken(EqualEqualToken.equalEqual, position)
object EqualEqualToken:
  final val equalEqual = "=="
case class LessEqualToken(override val position: SourcePosition) extends PunctuationToken(LessEqualToken.lessEqual, position)
object LessEqualToken:
  final val lessEqual = "<="
case class GreaterEqualToken(override val position: SourcePosition) extends PunctuationToken(GreaterEqualToken.greaterEqual, position)
object GreaterEqualToken:
  final val greaterEqual = ">="
case class LessToken(override val position: SourcePosition) extends PunctuationToken(LessToken.less, position)
object LessToken:
  final val less = "<"
case class GreaterToken(override val position: SourcePosition) extends PunctuationToken(GreaterToken.greater, position)
object GreaterToken:
  final val greater = ">"
case class SlashToken(override val position: SourcePosition) extends PunctuationToken(SlashToken.slash, position)
object SlashToken:
  final val slash = "/"
