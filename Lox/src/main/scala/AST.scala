package org.mikadocs.language.lox

import org.mikadocs.language.workbench.Node

trait ExpressionNode extends Node

case class UnaryNode(operator: MinusToken | BangToken, rightExpression: ExpressionNode) extends ExpressionNode

case class BinaryNode(operator: MinusToken | PlusToken | StarToken | SlashToken | EqualEqualToken | BangEqualToken |
                                GreaterToken | LessToken | GreaterEqualToken | LessEqualToken,
                      leftExpression: ExpressionNode,
                      rightExpression: ExpressionNode) extends ExpressionNode

case class LiteralNode(value: Double | String | Boolean | Null) extends ExpressionNode
