package org.mikadocs.language.kamin
package basic

import org.mikadocs.language.workbench.{IntegerValue, Node, NodeVisitor, visit}

import scala.annotation.tailrec
import scala.collection.mutable

object functionDefinitionTable:
  type Function = (Environment[IntegerValue], Seq[IntegerValue]) => Either[String, IntegerValue]
  case class FunctionDefinitionEntry(numberOfArguments: Int,
                                     function: Function)
  private val table = mutable.HashMap[String, FunctionDefinitionEntry]()

  table.put("+", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value + arguments(1).value))))
  table.put("-", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value - arguments(1).value))))
  table.put("*", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value * arguments(1).value))))
  table.put("/", FunctionDefinitionEntry(2, (env, arguments) =>
    if arguments(1).value != 0 then Right(IntegerValue(arguments.head.value / arguments(1).value)) else Left("Division with zero")))
  table.put("=", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value == arguments(1).value then 1 else 0
    ))))
  table.put("<", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value < arguments(1).value then 1 else 0
    ))))
  table.put(">", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value > arguments(1).value then 1 else 0
    ))))
  table.put("print", FunctionDefinitionEntry(1, (env, arguments) =>
    print(arguments.head.value)
    Right(arguments.head)))

  def register(functionDefinition: FunctionDefinitionNode): Unit =
    table.put(functionDefinition.function,
      FunctionDefinitionEntry(
        functionDefinition.arguments.length,
        (globalEnvironment, parameters) =>
          val environment = PredefinedEnvironmentFrame[IntegerValue](globalEnvironment, functionDefinition.arguments)
          functionDefinition.arguments.zip(parameters).foreach((k, v) => environment.set(k,v))
          BasicEvaluator(environment).visit(functionDefinition.expression)
      ))

  def lookupFunctionDefinition(name: String): Option[FunctionDefinitionEntry] =
    table.get(name)

class BasicEvaluator(val currentEnvironment: Environment[IntegerValue])
  extends NodeVisitor[Either[String, IntegerValue]]:
  private def evaluateParameters(parameters: Seq[ExpressionNode]
                                ): Either[String, List[IntegerValue]] =
    parameters.foldLeft(Right(List.empty[IntegerValue]): Either[String, List[IntegerValue]]) { (acc, p) =>
      acc match
        case Left(error) => Left(error) // If there's already an error, keep it
        case Right(params) =>
          visit(p) match
            case Left(error) => Left(error) // Stop and return the error if evaluation fails
            case Right(result) => Right(params :+ result) // Append result to the list if successful
    }

  @tailrec
  private def evaluateLoop(whileExpressionNode: WhileExpressionNode) : Either[String, IntegerValue] =
    visit(whileExpressionNode.test) match
      case Right(IntegerValue(0)) => Right(IntegerValue(0))
      case Right(_) =>
        visit(whileExpressionNode.body) match
          case Left(error) => Left(error)
          case Right(_) => evaluateLoop(whileExpressionNode)
      case Left(error) => Left(error)
      
  override def visit(node: Node): Either[String, IntegerValue] =
    node match
      case ValueExpressionNode(value) => Right(value)
      case VariableExpressionNode(variable: String) =>
        currentEnvironment.get(variable).toRight(s"Unknown variable: $variable")
      case IfExpressionNode(test, consequence, alternative) =>
        visit(test) match
          case Right(IntegerValue(0)) => visit(alternative)
          case Right(_) => visit(consequence)
          case Left(error) => Left(error)
      case n:WhileExpressionNode => evaluateLoop(n)
      case SetExpressionNode(variable, value) =>
        visit(value) match
          case Right(v) => 
            currentEnvironment.set(variable, v)
            Right(v)
          case Left(error) => Left(error)
      case BeginExpressionNode(expressions) =>
        expressions.map(e => visit(e)).last
      case OperationExpressionNode(operator, parameters) =>
        functionDefinitionTable.lookupFunctionDefinition(operator) match
          case None => Left(s"Unknown operator: $operator")
          case Some(functionDefinition) =>
            val evaluated = evaluateParameters(parameters)
            evaluated match
              case Left(error) => Left(error)
              case Right(parameters) =>
                if functionDefinition.numberOfArguments == parameters.length then
                  functionDefinition.function(currentEnvironment, parameters)
                else
                  Left(s"$operator: invalid number of arguments")

object BasicEvaluator:
  val globalEnvironment: Environment[IntegerValue] = GlobalEnvironment[IntegerValue]()

  def apply(env: Environment[IntegerValue]): BasicEvaluator =
    new BasicEvaluator(env)

  def default(): BasicEvaluator =
    new BasicEvaluator(globalEnvironment)

