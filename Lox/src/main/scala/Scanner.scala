package org.mikadocs.language.lox

import org.mikadocs.language.workbench.Acceptance.{Accepted, Rejected, Undecided}
import org.mikadocs.language.workbench.{Acceptance, RuleBasedScanner, StringMatchingRule, StringLookaheadMatchingRule, ScannerRule, SourcePosition, Token, WhiteSpaceSkippingScanner}

private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

object StringRule extends ScannerRule:
  override def accept(s: String): Acceptance =
    if """\"([^\"]*)\"""".r matches s then
      Accepted
    else if """\"([^\"]*)""".r matches s then
      Undecided
    else
      Rejected
  override def apply(s: String, position: SourcePosition): Token = StringToken(s.substring(1, s.length - 1), position)

object IdentifierRule extends ScannerRule:
  override def accept(s: String): Acceptance = if s.forall(isAlphaNumeric) then Accepted else Rejected
  override def apply(s: String, position: SourcePosition): Token = IdentifierToken(s, position)

object RealNumberRule extends ScannerRule:
  override def accept(s: String): Acceptance =
    if """\d+\.\d+""".r matches s then
      Acceptance.Accepted
    else if """\d+\.""".r matches s then
      Acceptance.Undecided
    else
      Acceptance.Rejected

  override def apply(s: String, position: SourcePosition): Token = NumberToken(s, position)

object IntegerNumberRule extends ScannerRule:
  override def accept(s: String): Acceptance = if """\d+""".r matches s then Acceptance.Accepted else Acceptance.Rejected
  override def apply(s: String, position: SourcePosition): Token = NumberToken(s, position)

abstract class PunctuationRule(val lexeme:String, factory: SourcePosition => Token)
  extends StringMatchingRule(lexeme, (s, p) => factory(p))

abstract class PunctuationLoookaheadRule(val lexeme: String, factory: SourcePosition => Token)
  extends StringLookaheadMatchingRule(lexeme, StringLookaheadMatchingRule.allSubstringsAsUndecided(lexeme), (s, p) => factory(p))
object DotRule extends PunctuationRule(DotToken.dot, p => DotToken(p))
object LeftParenthesisRule extends PunctuationRule(LeftParenthesisToken.leftParenthesis, p => LeftParenthesisToken(p))
object RightParenthesisRule extends PunctuationRule(RightParenthesisToken.rightParenthesis, p => RightParenthesisToken(p))
object LeftBraceRule extends PunctuationRule(LeftBraceToken.leftBrace, p => LeftBraceToken(p))
object RightBraceRule extends PunctuationRule(RightBraceToken.rightBrace, p => RightBraceToken(p))
object SemicolonRule extends PunctuationRule(SemicolonToken.semicolon, p => SemicolonToken(p))
object CommaRule extends PunctuationRule(CommaToken.comma, p => CommaToken(p))
object PlusRule extends PunctuationRule(PlusToken.plus, p => PlusToken(p))
object MinusRule extends PunctuationRule(MinusToken.minus, p => MinusToken(p))
object StarRule extends PunctuationRule(StarToken.star, p => StarToken(p))
object SlashRule extends PunctuationRule(SlashToken.slash, p => SlashToken(p))
object BangEqualRule extends PunctuationLoookaheadRule(BangEqualToken.bangEqual, p => BangEqualToken(p))
object EqualEqualRule extends PunctuationLoookaheadRule(EqualEqualToken.equalEqual, p => EqualEqualToken(p))
object LessEqualRule extends PunctuationLoookaheadRule(LessEqualToken.lessEqual, p => LessEqualToken(p))
object GreaterEqualRule extends PunctuationLoookaheadRule(GreaterEqualToken.greaterEqual, p => GreaterEqualToken(p))
object LessRule extends PunctuationRule(LessToken.less, p => LessToken(p))
object GreaterRule extends PunctuationRule(GreaterToken.greater, p => GreaterToken(p))

abstract class KeywordRule(val lexeme: String, factory: SourcePosition => Token)
  extends StringMatchingRule(lexeme, (s, p) => factory(p))

object AndRule extends KeywordRule(AndToken.and, p => AndToken(p))
object ClassRule extends KeywordRule(ClassToken._class, p => ClassToken(p))
object ElseRule extends KeywordRule(ElseToken._else, p => ElseToken(p))
object FalseRule extends KeywordRule(FalseToken._false, p => FalseToken(p))
object ForRule extends KeywordRule(ForToken._for, p => ForToken(p))
object FunRule extends KeywordRule(FunToken.fun, p => FunToken(p))
object IfRule extends KeywordRule(IfToken._if, p => IfToken(p))
object NilRule extends KeywordRule(NilToken.nil, p => NilToken(p))
object OrRule extends KeywordRule(OrToken.or, p => OrToken(p))
object ReturnRule extends KeywordRule(ReturnToken._return, p => ReturnToken(p))
object SuperRule extends KeywordRule(SuperToken._super, p => SuperToken(p))
object ThisRule extends KeywordRule(ThisToken._this, p => ThisToken(p))
object TrueRule extends KeywordRule(TrueToken._true, p => TrueToken(p))
object VarRule extends KeywordRule(VarToken._var, p => VarToken(p))
object WhileRule extends KeywordRule(WhileToken._while, p => WhileToken(p))

class LoxScanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  RealNumberRule,
  IntegerNumberRule,
  DotRule,
  LeftParenthesisRule,
  RightParenthesisRule,
  LeftBraceRule,
  RightBraceRule,
  SemicolonRule,
  CommaRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  BangEqualRule,
  EqualEqualRule,
  LessEqualRule,
  GreaterEqualRule,
  LessRule,
  GreaterRule,
  AndRule,
  ClassRule,
  ElseRule,
  FalseRule,
  ForRule,
  FunRule,
  IfRule,
  NilRule,
  OrRule,
  ReturnRule,
  SuperRule,
  ThisRule,
  TrueRule,
  VarRule,
  WhileRule,
  IdentifierRule,
  StringRule
)))
