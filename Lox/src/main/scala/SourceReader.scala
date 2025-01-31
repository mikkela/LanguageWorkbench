package org.mikadocs.language.lox

import org.mikadocs.language.workbench.{SourcePosition, SourceReader, SourceReading}

import scala.collection.mutable
import scala.collection.mutable.Queue

class LoxSourceReader(val internal: SourceReader) extends SourceReader:
  override def position: SourcePosition = internal.position

  override def read: SourceReading =
    val reading = LoxSourceReader.read(internal)
    SourceReading(reading.current, LoxSourceReader(reading.next))

  override def atEndOfSource: Boolean = internal.atEndOfSource

object LoxSourceReader:
  private val buffer = mutable.Queue[SourceReading]()

  def read(from: SourceReader): SourceReading =
    if buffer.isEmpty then
      var reading = from.read
      buffer.enqueue(reading)
      if reading.current == '/' then
        reading = reading.next.read
        buffer.enqueue(reading)
        if reading.current == '/' then
          buffer.clear()
          while reading.current != '\n' && !reading.next.atEndOfSource do
            reading = reading.next.read
          return if reading.next.atEndOfSource then reading.next.read else read(reading.next)

    buffer.dequeue()


