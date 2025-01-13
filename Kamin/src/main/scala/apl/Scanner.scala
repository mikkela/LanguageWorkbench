package org.mikadocs.language.kamin
package apl

import org.mikadocs.language.workbench.{RuleBasedScanner, WhiteSpaceSkippingScanner}

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  QuoteRule,
  DefineRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  EqualRule,
  LessRule,
  GreaterRule,
  MaxRule,
  AndRule,
  OrRule,
  PlusSlashRule,
  MinusSlashRule,
  StarSlashRule,
  SlashSlashRule,
  MaxSlashRule,
  AndSlashRule,
  OrSlashRule,
  CompressRule,
  ShapeRule,
  RavelRule,
  RestructRule,
  CatRule,
  IndxRule,
  TransRule,
  LeftSquareBracketRightSquareBracketRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, QuoteToken.quote))
)))



