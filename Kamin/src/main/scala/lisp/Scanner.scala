package org.mikadocs.language.kamin
package lisp

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
  ConsRule,
  CarRule,
  CdrRule,
  NumberQuestionRule,
  SymbolQuestionRule,
  ListQuestionRule,
  NullQuestionRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, QuoteToken.quote))
)))



