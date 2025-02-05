package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{IntegerValue, Node}

trait ExpressionNode extends Node

case class ValueExpressionNode(value: IntegerValue) extends ExpressionNode
case class VariableExpressionNode(variable: NameToken) extends ExpressionNode
case class IfExpressionNode(test:ExpressionNode, consequence: ExpressionNode, alternative: ExpressionNode) extends ExpressionNode
case class WhileExpressionNode(test:ExpressionNode, body: ExpressionNode) extends ExpressionNode
case class SetExpressionNode(variable: NameToken, value: ExpressionNode) extends ExpressionNode
case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode
case class OperationExpressionNode(operator: OperatorToken | NameToken, parameters: Seq[ExpressionNode]) extends ExpressionNode

case class FunctionDefinitionNode(function: NameToken, arguments: Seq[NameToken], expression: ExpressionNode) extends Node
