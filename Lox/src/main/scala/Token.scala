package org.mikadocs.language
package lox

import org.mikadocs.language.workbench.{SourcePosition, Token}

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
case class NumberToken(lexeme: String, position: SourcePosition) extends Token
case class StringToken(lexeme: String, position: SourcePosition) extends Token

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
case class BangToken(override val position: SourcePosition) extends PunctuationToken(BangToken.bang, position)
object BangToken:
  final val bang = "!"
case class EqualToken(override val position: SourcePosition) extends PunctuationToken(EqualToken.equal, position)
object EqualToken:
  final val equal = "="
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

abstract class KeywordToken(val lexeme: String, val position: SourcePosition) extends Token
case class AndToken(override val position: SourcePosition) extends KeywordToken(AndToken.and, position)
object AndToken:
  final val and = "and"
case class ClassToken(override val position: SourcePosition) extends KeywordToken(ClassToken._class, position)
object ClassToken:
  final val _class = "class"
case class ElseToken(override val position: SourcePosition) extends KeywordToken(ElseToken._else, position)
object ElseToken:
  final val _else = "else"
case class FalseToken(override val position: SourcePosition) extends KeywordToken(FalseToken._false, position)
object FalseToken:
  final val _false = "false"
case class ForToken(override val position: SourcePosition) extends KeywordToken(ForToken._for, position)
object ForToken:
  final val _for = "for"
case class FunToken(override val position: SourcePosition) extends KeywordToken(FunToken.fun, position)
object FunToken:
  final val fun = "fun"
case class IfToken(override val position: SourcePosition) extends KeywordToken(IfToken._if, position)
object IfToken:
  final val _if = "if"
case class NilToken(override val position: SourcePosition) extends KeywordToken(NilToken.nil, position)
object NilToken:
  final val nil = "nil"
case class OrToken(override val position: SourcePosition) extends KeywordToken(OrToken.or, position)
object OrToken:
  final val or = "or"
case class ReturnToken(override val position: SourcePosition) extends KeywordToken(ReturnToken._return, position)
object ReturnToken:
  final val _return = "return"
case class SuperToken(override val position: SourcePosition) extends KeywordToken(SuperToken._super, position)
object SuperToken:
  final val _super = "super"
case class ThisToken(override val position: SourcePosition) extends KeywordToken(ThisToken._this, position)
object ThisToken:
  final val _this = "this"
case class TrueToken(override val position: SourcePosition) extends KeywordToken(TrueToken._true, position)
object TrueToken:
  final val _true = "true"
case class VarToken(override val position: SourcePosition) extends KeywordToken(VarToken._var, position)
object VarToken:
  final val _var = "var"
case class WhileToken(override val position: SourcePosition) extends KeywordToken(WhileToken._while, position)
object WhileToken:
  final val _while = "while"
