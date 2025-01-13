package org.mikadocs.language.kamin
package prolog

import org.mikadocs.language.workbench.{EndOfFileToken, SourcePosition, SourceReader}

class ScannerTest extends munit.FunSuite{
  // Helper for creating a simple SourcePosition
  def createSourcePosition(l: Int, c: Int): SourcePosition = new SourcePosition {
    override def line: Int = l

    override def column: Int = c

    override def lineContents: String = ""
  }

  test("Scan for identifiers") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("andy formless fo _ _123 a_bc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_(==)>="))

    assertEquals(iterator.next(), NameToken("andy", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NameToken("formless", createSourcePosition(1, 6)))
    assertEquals(iterator.next(), NameToken("fo", createSourcePosition(1, 15)))
    assertEquals(iterator.next(), NameToken("_", createSourcePosition(1, 18)))
    assertEquals(iterator.next(), NameToken("_123", createSourcePosition(1, 20)))
    assertEquals(iterator.next(), NameToken("a_bc", createSourcePosition(1, 25)))
    assertEquals(iterator.next(), NameToken("ab123", createSourcePosition(1, 30)))
    assertEquals(iterator.next(), NameToken("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), LeftParenthesisToken(createSourcePosition(2, 64)))
    assertEquals(iterator.next(), NameToken("==", createSourcePosition(2, 65)))
    assertEquals(iterator.next(), RightParenthesisToken(createSourcePosition(2, 67)))
    assertEquals(iterator.next(), NameToken(">=", createSourcePosition(2, 68)))

    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for numbers") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("123\n-123\n--456\n123-"))
    assertEquals(iterator.next(), IntegerToken("123", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), IntegerToken("-123", createSourcePosition(2, 1)))
    assertEquals(iterator.next(), NameToken("--456", createSourcePosition(3, 1)))
    assertEquals(iterator.next(), NameToken("123-", createSourcePosition(4, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for punctuators") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("()'$#"))
    assertEquals(iterator.next(), LeftParenthesisToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), RightParenthesisToken(createSourcePosition(1, 2)))
    assertEquals(iterator.next(), NameToken("'$#", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for Operators") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("+ - * /(= < > print max or)and compress shape'restruct indx trans ravel [] +/ -/ */ // max/ or/ and/ cat cons car cdr number? symbol? list? null? primop? closure?"))
    assertEquals(iterator.next(), NameToken("+", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NameToken("-", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), NameToken("*", createSourcePosition(1, 5)))
    assertEquals(iterator.next(), NameToken("/", createSourcePosition(1, 7)))
    assertEquals(iterator.next(), LeftParenthesisToken(createSourcePosition(1, 8)))
    assertEquals(iterator.next(), NameToken("=", createSourcePosition(1, 9)))
    assertEquals(iterator.next(), NameToken("<", createSourcePosition(1, 11)))
    assertEquals(iterator.next(), NameToken(">", createSourcePosition(1, 13)))
    assertEquals(iterator.next(), NameToken("print", createSourcePosition(1, 15)))
    assertEquals(iterator.next(), NameToken("max", createSourcePosition(1, 21)))
    assertEquals(iterator.next(), NameToken("or", createSourcePosition(1, 25)))
    assertEquals(iterator.next(), RightParenthesisToken(createSourcePosition(1, 27)))
    assertEquals(iterator.next(), NameToken("and", createSourcePosition(1, 28)))
    assertEquals(iterator.next(), NameToken("compress", createSourcePosition(1, 32)))
    assertEquals(iterator.next(), NameToken("shape'restruct", createSourcePosition(1, 41)))
    assertEquals(iterator.next(), NameToken("indx", createSourcePosition(1, 56)))
    assertEquals(iterator.next(), NameToken("trans", createSourcePosition(1, 61)))
    assertEquals(iterator.next(), NameToken("ravel", createSourcePosition(1, 67)))
    assertEquals(iterator.next(), NameToken("[]", createSourcePosition(1, 73)))
    assertEquals(iterator.next(), NameToken("+/", createSourcePosition(1, 76)))
    assertEquals(iterator.next(), NameToken("-/", createSourcePosition(1, 79)))
    assertEquals(iterator.next(), NameToken("*/", createSourcePosition(1, 82)))
    assertEquals(iterator.next(), NameToken("//", createSourcePosition(1, 85)))
    assertEquals(iterator.next(), NameToken("max/", createSourcePosition(1, 88)))
    assertEquals(iterator.next(), NameToken("or/", createSourcePosition(1, 93)))
    assertEquals(iterator.next(), NameToken("and/", createSourcePosition(1, 97)))
    assertEquals(iterator.next(), NameToken("cat", createSourcePosition(1, 102)))
    assertEquals(iterator.next(), NameToken("cons", createSourcePosition(1, 106)))
    assertEquals(iterator.next(), NameToken("car", createSourcePosition(1, 111)))
    assertEquals(iterator.next(), NameToken("cdr", createSourcePosition(1, 115)))
    assertEquals(iterator.next(), NameToken("number?", createSourcePosition(1, 119)))
    assertEquals(iterator.next(), NameToken("symbol?", createSourcePosition(1, 127)))
    assertEquals(iterator.next(), NameToken("list?", createSourcePosition(1, 135)))
    assertEquals(iterator.next(), NameToken("null?", createSourcePosition(1, 141)))
    assertEquals(iterator.next(), NameToken("primop?", createSourcePosition(1, 147)))
    assertEquals(iterator.next(), NameToken("closure?", createSourcePosition(1, 155)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan and avoid for white spaces") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("space    tabs\t\t\t\tnewlines\n\n\n\n\nend"))

    assertEquals(iterator.next(), NameToken("space", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NameToken("tabs", createSourcePosition(1, 10)))
    assertEquals(iterator.next(), NameToken("newlines", createSourcePosition(1, 18)))
    assertEquals(iterator.next(), NameToken("end", createSourcePosition(6, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("Scan for keywords") {
    val sut = Scanner()

    val iterator = sut.scan(SourceReader("define if while begin set lambda cluster rep class infer from infer?"))
    assertEquals(iterator.next(), NameToken("define", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), NameToken("if", createSourcePosition(1, 8)))
    assertEquals(iterator.next(), NameToken("while", createSourcePosition(1, 11)))
    assertEquals(iterator.next(), NameToken("begin", createSourcePosition(1, 17)))
    assertEquals(iterator.next(), NameToken("set", createSourcePosition(1, 23)))
    assertEquals(iterator.next(), NameToken("lambda", createSourcePosition(1, 27)))
    assertEquals(iterator.next(), NameToken("cluster", createSourcePosition(1, 34)))
    assertEquals(iterator.next(), NameToken("rep", createSourcePosition(1, 42)))
    assertEquals(iterator.next(), NameToken("class", createSourcePosition(1, 46)))
    assertEquals(iterator.next(), InferToken(createSourcePosition(1, 52)))
    assertEquals(iterator.next(), FromToken(createSourcePosition(1, 58)))
    assertEquals(iterator.next(), InferQuestionToken(createSourcePosition(1, 63)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }
}
