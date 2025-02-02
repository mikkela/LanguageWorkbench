package org.mikadocs.language.lox

import munit.FunSuite

import scala.io.Source
import org.mikadocs.language.workbench.{SourceReader, Success}

class ExpressionParserTests extends FunSuite {
  test("should parse an expression correctly") {
    val source = 
      LoxSourceReader(SourceReader(Source.fromFile(getPath("expression.lox")).mkString))
      
    val scanner = LoxScanner()
    val ast = ExpressionParser.parse(scanner.scan(source))
    assertEquals(ast, Success(
      BinaryNode(
        PlusToken(createSourcePosition(2, 15)),
        GroupNode(
          BinaryNode(
            MinusToken(createSourcePosition(2, 4)),
            LiteralNode(5.0),
            GroupNode(
              BinaryNode(
                MinusToken(createSourcePosition(2, 9)),
                LiteralNode(3.0),
                LiteralNode(1.0)
              )
            )
          )
        ),
        UnaryNode(
          MinusToken(createSourcePosition(2, 17)),
          LiteralNode(1.0)
        )
      )
    ))
    println(ast)
  }
  
  def getPath(fileName: String): String =
    getClass.getResource(s"/$fileName").getPath
}
