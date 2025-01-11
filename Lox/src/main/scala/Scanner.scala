package org.mikadocs.language
package lox

import org.mikadocs.language.workbench.Acceptance.{Accepted, Rejected, Undecided}
import org.mikadocs.language.workbench.{Acceptance, RuleBasedScanner, PredefinedStringMatchingRule, PredefinedStringRequiringLookaheadMatchingRule, ScannerRule, SourcePosition, Token, WhiteSpaceSkippingScanner}

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

object DotRule extends PredefinedStringMatchingRule(DotToken.dot, p => DotToken(p))
object LeftParenthesisRule extends PredefinedStringMatchingRule(LeftParenthesisToken.leftParenthesis, p => LeftParenthesisToken(p))
object RightParenthesisRule extends PredefinedStringMatchingRule(RightParenthesisToken.rightParenthesis, p => RightParenthesisToken(p))
object LeftBraceRule extends PredefinedStringMatchingRule(LeftBraceToken.leftBrace, p => LeftBraceToken(p))
object RightBraceRule extends PredefinedStringMatchingRule(RightBraceToken.rightBrace, p => RightBraceToken(p))
object SemicolonRule extends PredefinedStringMatchingRule(SemicolonToken.semicolon, p => SemicolonToken(p))
object CommaRule extends PredefinedStringMatchingRule(CommaToken.comma, p => CommaToken(p))
object PlusRule extends PredefinedStringMatchingRule(PlusToken.plus, p => PlusToken(p))
object MinusRule extends PredefinedStringMatchingRule(MinusToken.minus, p => MinusToken(p))
object StarRule extends PredefinedStringMatchingRule(StarToken.star, p => StarToken(p))
object SlashRule extends PredefinedStringMatchingRule(SlashToken.slash, p => SlashToken(p))
object BangEqualRule extends PredefinedStringRequiringLookaheadMatchingRule(BangEqualToken.bangEqual, p => BangEqualToken(p))
object EqualEqualRule extends PredefinedStringRequiringLookaheadMatchingRule(EqualEqualToken.equalEqual, p => EqualEqualToken(p))
object LessEqualRule extends PredefinedStringRequiringLookaheadMatchingRule(LessEqualToken.lessEqual, p => LessEqualToken(p))
object GreaterEqualRule extends PredefinedStringRequiringLookaheadMatchingRule(GreaterEqualToken.greaterEqual, p => GreaterEqualToken(p))
object LessRule extends PredefinedStringMatchingRule(LessToken.less, p => LessToken(p))
object GreaterRule extends PredefinedStringMatchingRule(GreaterToken.greater, p => GreaterToken(p))

object AndRule extends PredefinedStringMatchingRule(AndToken.and, p => AndToken(p))
object ClassRule extends PredefinedStringMatchingRule(ClassToken._class, p => ClassToken(p))
object ElseRule extends PredefinedStringMatchingRule(ElseToken._else, p => ElseToken(p))
object FalseRule extends PredefinedStringMatchingRule(FalseToken._false, p => FalseToken(p))
object ForRule extends PredefinedStringMatchingRule(ForToken._for, p => ForToken(p))
object FunRule extends PredefinedStringMatchingRule(FunToken.fun, p => FunToken(p))
object IfRule extends PredefinedStringMatchingRule(IfToken._if, p => IfToken(p))
object NilRule extends PredefinedStringMatchingRule(NilToken.nil, p => NilToken(p))
object OrRule extends PredefinedStringMatchingRule(OrToken.or, p => OrToken(p))
object ReturnRule extends PredefinedStringMatchingRule(ReturnToken._return, p => ReturnToken(p))
object SuperRule extends PredefinedStringMatchingRule(SuperToken._super, p => SuperToken(p))
object ThisRule extends PredefinedStringMatchingRule(ThisToken._this, p => ThisToken(p))
object TrueRule extends PredefinedStringMatchingRule(TrueToken._true, p => TrueToken(p))
object VarRule extends PredefinedStringMatchingRule(VarToken._var, p => VarToken(p))
object WhileRule extends PredefinedStringMatchingRule(WhileToken._while, p => WhileToken(p))

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
