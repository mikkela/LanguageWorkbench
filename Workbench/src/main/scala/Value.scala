package org.mikadocs.language.workbench

trait Value

class IntegerValue(val value: Int) extends Value:
  override def toString: String = value.toString

object IntegerValue:
  def unapply(v: IntegerValue): Option[Int] = Some(v.value)
  
class DoubleValue(val value: Double) extends Value:
  override def toString: String = value.toString

object DoubleValue:
  def unapply(v: DoubleValue): Option[Double] = Some(v.value)
  
class StringValue(val value: String) extends Value:
  override def toString: String = s"""$value"""

object StringValue:
  def unapply(v: StringValue): Option[String] = Some(v.value)
  
abstract class BooleanValue protected(val value: Boolean) extends Value:
  protected def asString(bool: Boolean): String

  override def toString: String = asString(value)

object BooleanValue:
  def unapply(v: BooleanValue): Option[Boolean] = Some(v.value)
    
implicit class IntegerValueExtension(val i: Int) extends AnyVal:
  def toIntegerValue: IntegerValue = IntegerValue(i)
