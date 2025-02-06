package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{EvaluationResult, LookaheadIterator,
  Interpreter, Node, NodeVisitor, ParseAndPrintResult, SourceReader, visit}

object loxPrinter extends NodeVisitor[String]:
  override def visit(node: Node): String =
    val result = StringBuilder()
    node match
      case BinaryNode(operator, leftExpression, rightExpression) =>
        result.append(visit(leftExpression))
        result.append(operator.lexeme)
        result.append(visit(rightExpression))
      case GroupNode(expression) =>
        result.append("(")
        result.append(visit(expression))
        result.append(")")
      case UnaryNode(operator, expression) =>
        result.append(operator.lexeme)
        result.append(visit(expression))
      case LiteralNode(value) =>
        result.append(value.toString)
    result.toString()

object loxInterpreter extends Interpreter:

  override def interpret(progam: String): EvaluationResult = ???

  override def parseAndPrint(program: String): ParseAndPrintResult =
    (expressionParser.parse(LoxScanner().scan(LoxSourceReader(SourceReader(program))))).map(
      ast => ast.visit(using loxPrinter)
    )

