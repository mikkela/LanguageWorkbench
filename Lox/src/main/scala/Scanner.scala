package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{RuleBasedScanner, ScannerRule, SourcePosition, Token, WhiteSpaceSkippingScanner}

private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

object IdentifierRule extends ScannerRule:
  override def accept(s: String): Boolean = s.forall(isAlphaNumeric)
  override def apply(s: String, position: SourcePosition): Token = IdentifierToken(s, position)

class LoxScanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  IdentifierRule)))
