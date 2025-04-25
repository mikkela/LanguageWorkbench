package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{EvaluationResult, Interpreter, LookaheadIterator, Node, NodeVisitor, ParseAndPrintResult, SourceReader, Token, visit}

object basicPrinter extends NodeVisitor[String]:
  override def visit(node: Node): String =
    val result = StringBuilder()
    node match
      case ValueExpressionNode(value) =>
        result.append(value.value)
      case VariableExpressionNode(variable) =>
        result.append(variable)
      case IfExpressionNode(test, consequence, alternative) =>
        result.append("(if ")
        result.append(visit(test))
        result.append(" ")
        result.append(visit(consequence))
        result.append(" ")
        result.append(visit(alternative))
        result.append(")")
      case WhileExpressionNode(test, body) =>
        result.append("(while ")
        result.append(visit(test))
        result.append(" ")
        result.append(visit(body))
        result.append(")")
      case SetExpressionNode(variable, value) =>
        result.append("(set ")
        result.append(variable)
        result.append(" ")
        result.append(visit(value))
        result.append(")")
      case BeginExpressionNode(expressions) =>
        result.append("(begin")
        expressions.foreach(
          e =>
            result.append(" ")
            result.append(visit(e))
        )
        result.append(")")
      case OperationExpressionNode(operator, parameters) =>
        result.append("( ")
        result.append(operator)
        parameters.foreach(
          e =>
            result.append(" ")
            result.append(visit(e))
        )
        result.append(")")
      case FunctionDefinitionNode(function, arguments, expression) =>
        result.append("(define ")
        result.append(function)
        result.append(" (")
        var writtenFirst = false
        arguments.foreach(
          a =>
            if writtenFirst then result.append(" ")
            result.append(a)
            writtenFirst = true
        )
        result.append(") ")
        result.append(visit(expression))
        result.append(" )")
    result.toString()

object basicInterpreter extends Interpreter:

  override def interpret(program: String): EvaluationResult =
    basicParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program)))).map(
      ast => BasicEvaluator(BasicEvaluator.globalEnvironment).visit(ast) match {
        case Left(error) => error
        case Right(result) => result.toString
      }
    )

  override def parseAndPrint(program: String): ParseAndPrintResult =
    (basicParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program))))).map(
      ast => ast.visit(using basicPrinter)
    )


