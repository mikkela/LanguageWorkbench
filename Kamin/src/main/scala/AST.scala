package org.mikadocs.language.kamin

import org.mikadocs.language.workbench.{IntegerValue, Node, Value}

trait ExpressionNode extends Node

case class ValueExpressionNode(value: IntegerValue | SExpressionNode) extends ExpressionNode
case class SExpressionNode(value: IntegerValue | SymbolValue | Seq[SExpressionNode]) extends ExpressionNode:
  override def toString: String =
    value match {
      case v: IntegerValue => v.toString
      case v: SymbolValue => v.toString
      case v: Seq[SExpressionNode] => "(" + v.mkString(" ") + ")"
    }

case class VariableExpressionNode(variable: String) extends ExpressionNode
case class IfExpressionNode(test:ExpressionNode, consequence: ExpressionNode, alternative: ExpressionNode) extends ExpressionNode
case class WhileExpressionNode(test:ExpressionNode, body: ExpressionNode) extends ExpressionNode
case class SetExpressionNode(variable: String, value: ExpressionNode) extends ExpressionNode
case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode
case class OperationExpressionNode(operator: String, parameters: Seq[ExpressionNode]) extends ExpressionNode
case class FunctionDefinitionNode(function: String, arguments: Seq[String], expression: ExpressionNode) extends Node

