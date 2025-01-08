package org.mikadocs.language.lox

import org.mikadocs.language.workbench.Acceptance.{Accepted, Rejected}
import org.mikadocs.language.workbench.{Acceptance, RuleBasedScanner, ScannerRule, SourcePosition, Token, WhiteSpaceSkippingScanner}

private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

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

object DotRule extends ScannerRule:
  override def accept(s: String): Acceptance = if s.equals(".") then Acceptance.Accepted else Acceptance.Rejected
  override def apply(s: String, position: SourcePosition): Token = DotToken(s, position)

class LoxScanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  RealNumberRule,
  IntegerNumberRule,
  DotRule,
  IdentifierRule)))
