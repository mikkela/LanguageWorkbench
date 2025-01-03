package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{SourcePosition, Token}

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
