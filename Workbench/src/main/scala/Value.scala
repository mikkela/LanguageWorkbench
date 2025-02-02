package org.mikadocs.language.workbench

trait Value

class IntegerValue(val value: Int) extends Value:
  override def toString: String = value.toString

class DoubleValue(val value: Double) extends Value:
  override def toString: String = value.toString

class StringValue(val value: String) extends Value:
  override def toString: String = s"""$value"""

abstract class BooleanValue protected(val value: Boolean) extends Value:
  protected def asString(bool: Boolean): String

  override def toString: String = asString(value)

