package org.mikadocs.language.workbench

class Scanner():
  def scan(in: String): Iterator[Token] = scan(in.linesIterator)
  def scan(in: Iterator[String]): Iterator[Token] =
    ???


trait ScannerRule
