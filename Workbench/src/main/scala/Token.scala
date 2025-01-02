package org.mikadocs.language.workbench

trait Token:
  def lexeme: String
  def position: SourcePosition

object EndOfFileToken extends Token:
  override def position: SourcePosition = ???
  override def lexeme: String = ???

case class ErrorToken(lexeme: String, position: SourcePosition) extends Token

