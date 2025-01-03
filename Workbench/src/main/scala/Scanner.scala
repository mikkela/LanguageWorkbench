package org.mikadocs.language.workbench

trait Scanner:
  def scan(source: SourceReader): Iterator[Token]

abstract class FilteringScanner(val scanner: Scanner, val predicate: Token => Boolean) extends Scanner:
  private class TokenIterator(tokenIterator: Iterator[Token]) extends Iterator[Token]:
    private var nextToken: Option[Token] = None

    // Ensure `nextToken` is always the next valid token (not a WhiteSpaceToken)
    private def advance(): Unit =
      while (tokenIterator.hasNext && nextToken.isEmpty) {
        val token = tokenIterator.next()
        if predicate(token) then nextToken = Some(token)
      }

    override def hasNext: Boolean =
      if nextToken.isEmpty then advance()
      nextToken.isDefined

    override def next(): Token =
      val result = if !hasNext then EndOfFileToken else nextToken.get
      nextToken = None
      result

  override def scan(source: SourceReader): Iterator[Token] = TokenIterator(scanner.scan(source))

class WhiteSpaceSkippingScanner(scanner: Scanner) extends FilteringScanner(scanner, token => !token.isInstanceOf[WhiteSpaceToken])

class RuleBasedScanner(ruleSet: Seq[ScannerRule]) extends Scanner:
  val rules = Seq(NewlineScannerRule, TabScannerRule, SpaceScannerRule) ++ ruleSet
  private class TokenIterator(var sourceReader: SourceReader) extends Iterator[Token]:
    private var hasMore = true
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
    override def hasNext: Boolean  = hasMore
    override def next(): Token =
      if sourceReader.atEndOfSource then
        hasMore = false
        return EndOfFileToken

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

object SpaceScannerRule extends ScannerRule:
  override def accept(s: String): Boolean = s == " "
  override def apply(s: String, position: SourcePosition): Token = SpaceToken(position)

object TabScannerRule extends ScannerRule:
  override def accept(s: String): Boolean = s == "\t"
  override def apply(s: String, position: SourcePosition): Token = TabToken(position)

object NewlineScannerRule extends ScannerRule:
  override def accept(s: String): Boolean = s == "\n"
  override def apply(s: String, position: SourcePosition): Token = NewlineToken(position)
