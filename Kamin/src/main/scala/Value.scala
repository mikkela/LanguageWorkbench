package org.mikadocs.language.kamin

import org.mikadocs.language.workbench.{IntegerValue, Value}

case class SymbolValue(val value: String) extends Value:
  override def toString: String = value

object SymbolValue:
  val T = SymbolValue("T")
  
case class ListValue(val value: List[Value]) extends Value:
  override def toString: String = "(" + value.mkString(" ") + ")"

implicit class SymbolValueExtension(val s: String) extends AnyVal:
  def toSymbolValue: SymbolValue = SymbolValue(s)
  
object ListValue:
  val nil = ListValue(List.empty)