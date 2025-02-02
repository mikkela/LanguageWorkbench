package org.mikadocs.language.workbench

import munit.FunSuite

import scala.reflect.ClassTag

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
case class NumberToken(lexeme: String, position: SourcePosition) extends Token


class TestParser extends Parser[Node]:
  override def parse(tokens: Iterator[Token]): ParserResult[Node] =
    Failure("Not implemented")

  // Exposing parseError for testing
  def testParseError(t: Token): ParserResult[Node] =
    parseError(t)
  // Exposing matchToken for testing
  def testMatchToken[T <: Token](tokens: scala.collection.BufferedIterator[Token])(using ct: ClassTag[T]): Option[T] =
    matchToken[T](tokens)

  def testMatchToken[T1 <: Token, T2 <: Token](tokens: scala.collection.BufferedIterator[Token])
                                              (using ct1: ClassTag[T1])
                                              (using ct2: ClassTag[T2]): Option[T1 | T2] =
    matchToken[T1, T2](tokens)

class ParserTests extends FunSuite {
  private def buffered(tokens: Token*): scala.collection.BufferedIterator[Token] =
    tokens.iterator.buffered

  test("matchToken should match the correct token type") {
    val tokens = buffered(IdentifierToken("x", createSourcePosition(1, 1)), NumberToken("42", createSourcePosition(1, 2)))
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, Some(IdentifierToken("x", createSourcePosition(1, 1))))
    assertEquals(tokens.head, NumberToken("42", createSourcePosition(1, 2))) // Ensure token was consumed
  }

  test("matchToken should match the correct token types if more than one") {
    val tokens = buffered(IdentifierToken("x", createSourcePosition(1, 1)), NumberToken("42", createSourcePosition(1, 2)))
    val parser = new TestParser

    val result = parser.testMatchToken[NumberToken, IdentifierToken](tokens)

    assertEquals(result, Some(IdentifierToken("x", createSourcePosition(1, 1))))
    assertEquals(tokens.head, NumberToken("42", createSourcePosition(1, 2))) // Ensure token was consumed
  }

  test("matchToken should return None if the token does not match") {
    val tokens = buffered(NumberToken("42", createSourcePosition(1, 1)))
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, None)
    assertEquals(tokens.head, NumberToken("42", createSourcePosition(1, 1))) // Ensure token was NOT consumed
  }

  test("matchToken should return None if no tokens are available") {
    val tokens = buffered()
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, None)
  }

  test("parseError should return error message for non-EOF token") {
    val parser = new TestParser
    val error = parser.testParseError(IdentifierToken("x", createSourcePosition(1, 1)))

    assertEquals(error, Failure("Parse error: x at position: 1.1"))
  }

  test("parseError should return end-of-file error message") {
    val parser = new TestParser
    val error = parser.testParseError(EndOfFileToken)

    assertEquals(error, Failure("Parse Error: Unexpected end of file"))
  }
}
