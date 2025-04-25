package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{IntegerValue, Node}

case class ValueExpressionNode(value: IntegerValue) extends ExpressionNode
case class VariableExpressionNode(variable: String) extends ExpressionNode
case class IfExpressionNode(test:ExpressionNode, consequence: ExpressionNode, alternative: ExpressionNode) extends ExpressionNode
case class WhileExpressionNode(test:ExpressionNode, body: ExpressionNode) extends ExpressionNode
case class SetExpressionNode(variable: String, value: ExpressionNode) extends ExpressionNode
case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode
case class OperationExpressionNode(operator: String, parameters: Seq[ExpressionNode]) extends ExpressionNode
case class FunctionDefinitionNode(function: String, arguments: Seq[String], expression: ExpressionNode) extends Node
