package org.mikadocs.language.kamin
package prolog

import org.mikadocs.language.workbench.{RuleBasedScanner, WhiteSpaceSkippingScanner}

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  InferRule,
  FromRule,
  InferQuestionRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis))
)))



