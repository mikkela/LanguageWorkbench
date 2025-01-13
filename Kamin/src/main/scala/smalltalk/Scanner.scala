package org.mikadocs.language.kamin
package smalltalk

import org.mikadocs.language.workbench.{RuleBasedScanner, WhiteSpaceSkippingScanner}

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  HashRule,
  DefineRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  ClassRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  EqualRule,
  LessRule,
  GreaterRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, HashToken.hash))
)))



