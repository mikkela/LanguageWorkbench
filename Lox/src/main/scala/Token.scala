package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{SourcePosition, Token}

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
case class NumberToken(lexeme: String, position: SourcePosition) extends Token
case class DotToken(lexeme: String, position: SourcePosition) extends Token
