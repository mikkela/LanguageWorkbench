package org.mikadocs.language.workbench

trait Value

case class IntegerValue(value: Int) extends Value, Ordered[IntegerValue]:
  override def toString: String = value.toString

  override def compare(that: IntegerValue): Int =
    this.value.compare(that.value)

object IntegerValue:
  def unapply(v: IntegerValue): Option[Int] = Some(v.value)

implicit class IntegerValueExtension(val i: Int) extends AnyVal:
  def toIntegerValue: IntegerValue = IntegerValue(i)

case class DoubleValue(value: Double) extends Value:
  override def toString: String = value.toString

object DoubleValue:
  def unapply(v: DoubleValue): Option[Double] = Some(v.value)
  
case class StringValue(value: String) extends Value:
  override def toString: String = s"""$value"""


