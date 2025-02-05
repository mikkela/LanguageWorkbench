package org.mikadocs.language

import workbench.{Interpreter, ParseAndPrintResult}

import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

val languageMap: Map[String, Interpreter] = Map(
  "kamin.basic" -> kamin.basic.basicInterpreter,
  "lox" -> lox.loxInterpreter
)

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
    if input.startsWith(":") then
      val parts = input.split("\\s+")
      if !parts.isEmpty then
        program.clear()
        parts.head match
          case ":exit" =>
            continue = false
          case ":language" =>
            if input.length == 2 then
              if languageMap.contains(parts(1)) then
                interpreter = languageMap(parts(1))
          case ":mode" =>
            if parts.length== 2 then
              parts(1) match
                case "parse" => parseOrEvaluate = Left(())
                case "evaluate" => parseOrEvaluate = Right(())
    else
      program.append(input)
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

