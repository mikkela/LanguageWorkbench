package org.mikadocs.language.kamin
package lisp

import org.mikadocs.language.workbench.{EvaluationResult, IntegerValue, Interpreter, LookaheadIterator, Node, NodeVisitor, ParseAndPrintResult, SourceReader, Token, visit}

object lispPrinter extends Printer:
  private val valueExpressionPrinter = ValueExpressionPrinter(this)
  private val variableExpressionPrinter = VariableExpressionPrinter()
  private val ifExpressionPrinter = IfExpressionPrinter(this)
  private val whileExpressionPrinter = WhileExpressionPrinter(this)
  private val setExpressionPrinter = SetExpressionPrinter(this)
  private val beginExpressionPrinter = BeginExpressionPrinter(this)
  private val operationExpressionPrinter = OperationExpressionPrinter(this)
  private val functionDefinitionPrinter = FunctionDefinitionPrinter(this)
  private val sExpressionPrinter = SExpressionPrinter(this)
  
  override def visit(node: Node): String =
    val result = StringBuilder()
    node match
      case n:ValueExpressionNode =>
        valueExpressionPrinter.printNodeTo(n, result)
      case n:VariableExpressionNode =>
        variableExpressionPrinter.printNodeTo(n, result)
      case n:IfExpressionNode =>
        ifExpressionPrinter.printNodeTo(n, result)
      case n:WhileExpressionNode =>
        whileExpressionPrinter.printNodeTo(n, result)
      case n:SetExpressionNode =>
        setExpressionPrinter.printNodeTo(n, result)
      case n:BeginExpressionNode =>
        beginExpressionPrinter.printNodeTo(n, result)
      case n:OperationExpressionNode =>
        operationExpressionPrinter.printNodeTo(n, result)
      case n:FunctionDefinitionNode =>
        functionDefinitionPrinter.printNodeTo(n, result)
      case n:SExpressionNode =>
        result.append("'")
        sExpressionPrinter.printNodeTo(n, result)
    result.toString()

object lispInterpreter extends Interpreter:

  override def interpret(program: String): EvaluationResult =
    lispParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program)))).map(
      ast => LispEvaluator(LispEvaluator.globalEnvironment).visit(ast) match {
        case Left(error) => error
        case Right(result) => result.toString
      }
    )

  override def parseAndPrint(program: String): ParseAndPrintResult =
    lispParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program)))).map(
      ast => ast.visit(using lispPrinter)
    )


