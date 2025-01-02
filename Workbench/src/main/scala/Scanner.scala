package org.mikadocs.language.workbench

trait Scanner:
  def scan(source: SourceReader): Iterator[Token]

class RuleBasedScanner(val rules: Seq[ScannerRule]) extends Scanner:
  private class TokenIterator(var sourceReader: SourceReader) extends Iterator[Token]:
    private def readWhile(condition: String => Boolean): String =
      val sb = StringBuilder()
      var reading: SourceReading = null
      while
        reading = sourceReader.read
        sb += reading.current
        condition(sb.toString()) && !sourceReader.atEndOfSource
      do sourceReader = reading.next
      sb.setLength(sb.length - 1)
      sb.toString()

    private def readLexeme(): String =
      readWhile(lexeme => rules.exists(_.accept(lexeme)))

    private def readUnrecognizedCharacters(): String =
      readWhile(lexeme => !rules.exists(_.accept(lexeme.substring(lexeme.length - 1))))
    override def hasNext: Boolean  = !sourceReader.atEndOfSource
    override def next(): Token =
      if !hasNext then return EndOfFileToken

      val position = sourceReader.position
      val lexeme = readLexeme()
      if lexeme.nonEmpty then
        rules.find(_.accept(lexeme)).map(_.apply(lexeme, position)).getOrElse(ErrorToken(lexeme, position))
      else
        ErrorToken(readUnrecognizedCharacters(), position)
  
  override def scan(source: SourceReader): Iterator[Token] =
    new TokenIterator(source)

trait ScannerRule:
  def accept(s: String): Boolean
  def apply(s: String, position: SourcePosition): Token
