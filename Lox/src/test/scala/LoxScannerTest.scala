package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{EndOfFileToken, SourcePosition, SourceReader}

class LoxScannerTest extends munit.FunSuite{
  // Helper for creating a simple SourcePosition
  def createSourcePosition(l: Int, c: Int): SourcePosition = new SourcePosition {
    override def line: Int = l

    override def column: Int = c

    override def lineContents: String = ""
  }

  test("Scan for identifiers") {
    val sut = LoxScanner()

    val iterator = sut.scan(SourceReader("andy formless fo _ _123 a_bc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"))

    assertEquals(iterator.next(), IdentifierToken("andy", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), IdentifierToken("formless", createSourcePosition(1, 6)))
    assertEquals(iterator.next(), IdentifierToken("fo", createSourcePosition(1, 15)))
    assertEquals(iterator.next(), IdentifierToken("_", createSourcePosition(1, 18)))
    assertEquals(iterator.next(), IdentifierToken("_123", createSourcePosition(1, 20)))
    assertEquals(iterator.next(), IdentifierToken("a_bc", createSourcePosition(1, 25)))
    assertEquals(iterator.next(), IdentifierToken("ab123", createSourcePosition(1, 30)))
    assertEquals(iterator.next(), IdentifierToken("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for numbers") {
    val sut = LoxScanner()

    val iterator = sut.scan(SourceReader("123\n123.456\n.456\n123."))
    assertEquals(iterator.next(), NumberToken("123", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NumberToken("123.456", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(3, 1)))
    assertEquals(iterator.next(), NumberToken("456", createSourcePosition(3, 2)))
    assertEquals(iterator.next(), NumberToken("123", createSourcePosition(4, 1)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(4, 4)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for punctuators") {
    val sut = LoxScanner()

    val iterator = sut.scan(SourceReader("(){};,+-*!===<=>=!=<>/."))
    assertEquals(iterator.next(), LeftParenthesisToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), RightParenthesisToken(createSourcePosition(1, 2)))
    assertEquals(iterator.next(), LeftBraceToken(createSourcePosition(1, 3)))
    assertEquals(iterator.next(), RightBraceToken(createSourcePosition(1, 4)))
    assertEquals(iterator.next(), SemicolonToken(createSourcePosition(1, 5)))
    assertEquals(iterator.next(), CommaToken(createSourcePosition(1, 6)))
    assertEquals(iterator.next(), PlusToken(createSourcePosition(1, 7)))
    assertEquals(iterator.next(), MinusToken(createSourcePosition(1, 8)))
    assertEquals(iterator.next(), StarToken(createSourcePosition(1, 9)))
    assertEquals(iterator.next(), BangEqualToken(createSourcePosition(1, 10)))
    assertEquals(iterator.next(), EqualEqualToken(createSourcePosition(1, 12)))
    assertEquals(iterator.next(), LessEqualToken(createSourcePosition(1, 14)))
    assertEquals(iterator.next(), GreaterEqualToken(createSourcePosition(1, 16)))
    assertEquals(iterator.next(), BangEqualToken(createSourcePosition(1, 18)))
    assertEquals(iterator.next(), LessToken(createSourcePosition(1, 20)))
    assertEquals(iterator.next(), GreaterToken(createSourcePosition(1, 21)))
    assertEquals(iterator.next(), SlashToken(createSourcePosition(1, 22)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(1, 23)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }
}
