package org.mikadocs.language
package lox

import org.mikadocs.language.workbench.{EndOfFileToken, SourcePosition, SourceReader}

// Helper for creating a simple SourcePosition
def createSourcePosition(l: Int, c: Int): SourcePosition = new SourcePosition:
  override def line: Int = l

  override def column: Int = c

  override def lineContents: String = ""

class LoxScannerTest extends munit.FunSuite{
  test("Scan for identifiers") {
    val sut = LoxScanner()

    val iterator = sut.scan(LoxSourceReader(SourceReader("andy formless fo _ _123 a_bc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_")))

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

    val iterator = sut.scan(LoxSourceReader(SourceReader("(4 123\n123.456\n.456\n123. 3 5")))
    assertEquals(iterator.next(), LeftParenthesisToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NumberToken("4", createSourcePosition(1, 2)))
    assertEquals(iterator.next(), NumberToken("123", createSourcePosition(1, 4)))
    assertEquals(iterator.next(), NumberToken("123.456", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(3, 1)))
    assertEquals(iterator.next(), NumberToken("456", createSourcePosition(3, 2)))
    assertEquals(iterator.next(), NumberToken("123", createSourcePosition(4, 1)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(4, 4)))
    assertEquals(iterator.next(), NumberToken("3", createSourcePosition(4, 6)))
    assertEquals(iterator.next(), NumberToken("5", createSourcePosition(4, 8)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for punctuators") {
    val sut = LoxScanner()

    val iterator = sut.scan(LoxSourceReader(SourceReader("(){};,+-*!===<=>=! = <>/.")))
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
    assertEquals(iterator.next(), BangToken(createSourcePosition(1, 18)))
    assertEquals(iterator.next(), EqualToken(createSourcePosition(1, 20)))
    assertEquals(iterator.next(), LessToken(createSourcePosition(1, 22)))
    assertEquals(iterator.next(), GreaterToken(createSourcePosition(1, 23)))
    assertEquals(iterator.next(), SlashToken(createSourcePosition(1, 24)))
    assertEquals(iterator.next(), DotToken(createSourcePosition(1, 25)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for strings") {
    val sut = LoxScanner()

    val iterator = sut.scan(LoxSourceReader(SourceReader("\"\"\n\"string\"")))

    assertEquals(iterator.next(), StringToken("", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), StringToken("string", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan and avoid for white spaces") {
    val sut = LoxScanner()

    val iterator = sut.scan(SourceReader("space    tabs\t\t\t\tnewlines\n\n\n\n\nend"))

    assertEquals(iterator.next(), IdentifierToken("space", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), IdentifierToken("tabs", createSourcePosition(1, 10)))
    assertEquals(iterator.next(), IdentifierToken("newlines", createSourcePosition(1, 18)))
    assertEquals(iterator.next(), IdentifierToken("end", createSourcePosition(6, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for keywords") {
    val sut = LoxScanner()

    val iterator = sut.scan(LoxSourceReader(SourceReader("and class else false for fun if nil or return super this true var while")))
    assertEquals(iterator.next(), AndToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), ClassToken(createSourcePosition(1, 5)))
    assertEquals(iterator.next(), ElseToken(createSourcePosition(1, 11)))
    assertEquals(iterator.next(), FalseToken(createSourcePosition(1, 16)))
    assertEquals(iterator.next(), ForToken(createSourcePosition(1, 22)))
    assertEquals(iterator.next(), FunToken(createSourcePosition(1, 26)))
    assertEquals(iterator.next(), IfToken(createSourcePosition(1, 30)))
    assertEquals(iterator.next(), NilToken(createSourcePosition(1, 33)))
    assertEquals(iterator.next(), OrToken(createSourcePosition(1, 37)))
    assertEquals(iterator.next(), ReturnToken(createSourcePosition(1, 40)))
    assertEquals(iterator.next(), SuperToken(createSourcePosition(1, 47)))
    assertEquals(iterator.next(), ThisToken(createSourcePosition(1, 53)))
    assertEquals(iterator.next(), TrueToken(createSourcePosition(1, 58)))
    assertEquals(iterator.next(), VarToken(createSourcePosition(1, 63)))
    assertEquals(iterator.next(), WhileToken(createSourcePosition(1, 67)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }
}
