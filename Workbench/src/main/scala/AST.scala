package org.mikadocs.language.workbench

trait Node

trait NodeVisitor[T]:
  def visit(node: Node): T

extension [T](node: Node) def visit(using v: NodeVisitor[T]): T = v.visit(node)
