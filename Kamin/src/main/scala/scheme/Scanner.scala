package org.mikadocs.language.kamin
package scheme

import org.mikadocs.language.workbench.{RuleBasedScanner, WhiteSpaceSkippingScanner}

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  QuoteRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  LambdaRule,
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
  PrimopQuestionRule,
  ClosureQuestionRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, QuoteToken.quote))
)))



