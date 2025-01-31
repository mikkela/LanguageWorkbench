package org.mikadocs.language.workbench

// Generic Registry
class Registry[K, V]:
  private val registry: scala.collection.mutable.Map[K, V] = scala.collection.mutable.Map()

  def register(key: K, value: V): Unit = 
    registry += (key -> value)

  def get(key: K): Option[V] = 
    registry.get(key)

  def size: Int = registry.size

sealed trait TriState
case object True extends TriState
case object False extends TriState
case object NotApplicable extends TriState
