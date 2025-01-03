package org.mikadocs.language.workbench

trait Token:
  def lexeme: String
  def position: SourcePosition

object EndOfFileToken extends Token:
  override def position: SourcePosition = ???
  override def lexeme: String = ???

case class ErrorToken(lexeme: String, position: SourcePosition) extends Token
trait WhiteSpaceToken extends Token
case class SpaceToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = " "
case class TabToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = "\t"
case class NewlineToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = "\n"
