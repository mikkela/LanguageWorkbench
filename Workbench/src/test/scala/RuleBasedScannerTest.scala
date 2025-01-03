package org.mikadocs.language.workbench

class RuleBasedScannerTest extends munit.FunSuite {
  // Helper case classes for test tokens and positions
  case class TestToken(lexeme: String, position: SourcePosition) extends Token

  // Helper for creating a simple SourcePosition
  def createSourcePosition(l: Int, c: Int): SourcePosition = new SourcePosition {
    override def line: Int = l

    override def column: Int = c

    override def lineContents: String = ""
  }

  // Helper for creating a ScannerRule
  def createRule(
                  accepts: String => Boolean,
                  transform: String => String = identity
                ): ScannerRule = new ScannerRule {
    override def accept(s: String): Boolean = accepts(s)

    override def apply(s: String, position: SourcePosition): Token =
      if accept(s) then TestToken(transform(s), position) else ErrorToken(s, position)
  }
  
  test("scans the provided stream with a single matching rule") {
    val sut = RuleBasedScanner(Seq(createRule(_.startsWith("a"))))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with mutually exclusive rules") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("other".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken("other", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("selects the longest match when multiple rules apply") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("another".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("selects the first matching rule when matches are equal") {
    val sut = RuleBasedScanner(Seq(
      createRule("another".contains, _ + "!"),
      createRule("another".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another!", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a single non-matching rule") {
    val sut = RuleBasedScanner(Seq(createRule(_.startsWith("b"))))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), ErrorToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a two non-matching rule where the latter matches later in the string") {
    val sut = RuleBasedScanner(Seq(
      createRule(_.startsWith("b")),
      createRule("other".contains),
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), ErrorToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken("other", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a first a matching then non-matching rules and then matching again") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("y".contains),
      createRule("her".contains),
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), ErrorToken("ot", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), TestToken("her", createSourcePosition(1, 5)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans empty stream with a single matching rule") {
    val sut = RuleBasedScanner(Seq(createRule(_ => true)))
    val iterator = sut.scan(SourceReader(""))

    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
    assertEquals(iterator.next(), EndOfFileToken)
  }

  test("scans whitespaces into relevant tokens by default") {
    val sut = RuleBasedScanner(Seq.empty)
    val iterator = sut.scan(SourceReader(" \t \n "))

    assertEquals(iterator.next(), SpaceToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TabToken(createSourcePosition(1, 2)))
    assertEquals(iterator.next(), SpaceToken(createSourcePosition(1, 3)))
    assertEquals(iterator.next(), NewlineToken(createSourcePosition(1, 4)))
    assertEquals(iterator.next(), SpaceToken(createSourcePosition(2, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }
}
