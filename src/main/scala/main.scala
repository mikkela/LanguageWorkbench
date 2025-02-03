package org.mikadocs.language

import lox.loxInterpreter
import workbench.{Interpreter, ParseAndPrintResult}

import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

val languageMap: Map[String, Interpreter] = Map("lox" -> loxInterpreter)

@main
def Workbench(): Unit =
  val terminal = TerminalBuilder.terminal()
  val lineReader = LineReaderBuilder.builder()
    .terminal(terminal)
    .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
    .build()

  var interpreter = languageMap.head._2
  var continue = true
  var parseOrEvaluate: Either[Unit, Unit] = Left(())
  var program = StringBuilder()
  while continue do
    val input = (if program.isEmpty then lineReader.readLine("->") else lineReader.readLine(">"))

      .split("\\s+")
    if !input.isEmpty then
      input.head match
        case ":exit" =>
          program.clear()
          continue = false
        case ":language" =>
          program.clear()
          if input.length == 2 then
            if languageMap.contains(input(1)) then
              interpreter = languageMap(input(1))
        case ":mode" =>
          program.clear()
          if input.length== 2 then
            input(1) match
              case "parse" => parseOrEvaluate = Left(())
              case "evaluate" => parseOrEvaluate = Right(())
        case s =>
          program.append(s)
          if parseOrEvaluate.isLeft then
            interpreter.parseAndPrint(program.toString()).handle(
              v => {
                program.clear()
                println(v)
              },
              e => {
                program.clear()
                println("Error: " + e)
              }
            )
          else
            interpreter.parseAndPrint(program.toString()).handle(
              v => {
                program.clear()
                println(v)
              },
              e => {
                program.clear()
                println("Error: " + e)
              }
            )

