package org.mikadocs.language.workbench
import org.mikadocs.language.workbench

import scala.annotation.targetName
import scala.reflect.ClassTag

type ParserResult[T <: Node] = Result[T]

trait Parser[T <: Node]:
  def parse(tokens: Iterator[Token]): ParserResult[T]

  protected def parseError(token: Token): ParserResult[T] =
    if token != EndOfFileToken then
      Failure(s"${this.getClass.getSimpleName}: Parse error: ${token.lexeme} at position: ${token.position}")
    else
      Failure(s"${this.getClass.getSimpleName}: Parse Error: Unexpected end of file")

  @targetName("matchToken")
  protected def matchToken[TokenType <: Token](tokens: scala.collection.BufferedIterator[Token])
                                              (using ct: ClassTag[TokenType]): Option[TokenType] =
    if !tokens.hasNext then None
    else if ct.runtimeClass.isInstance(tokens.head) then
      Some(tokens.next().asInstanceOf[TokenType])
    else
      None

  @targetName("match2Token")
  protected def matchToken[TokenTypeAlternative1 <: Token, TokenTypeAlternative2 <: Token](tokens: scala.collection.BufferedIterator[Token])
                                                                                          (using ct1: ClassTag[TokenTypeAlternative1])
                                                                                          (using ct2: ClassTag[TokenTypeAlternative2]): Option[TokenTypeAlternative1 | TokenTypeAlternative2] =
    matchToken[TokenTypeAlternative1](tokens).
      orElse(matchToken[TokenTypeAlternative2](tokens))

  @targetName("match3Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token
  ]
  (tokens: scala.collection.BufferedIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2](tokens).
      orElse(matchToken[TokenTypeAlternative3](tokens))

  @targetName("match4Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token
  ]
  (tokens: scala.collection.BufferedIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3|TokenTypeAlternative4] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3](tokens).
      orElse(matchToken[TokenTypeAlternative4](tokens))

  @targetName("match5Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token,
    TokenTypeAlternative5 <: Token
  ]
  (tokens: scala.collection.BufferedIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4])
  (using ct5: ClassTag[TokenTypeAlternative5]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3 | TokenTypeAlternative4 | TokenTypeAlternative5] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3, TokenTypeAlternative4](tokens).
      orElse(matchToken[TokenTypeAlternative5](tokens))
  @targetName("match6Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token,
    TokenTypeAlternative5 <: Token,
    TokenTypeAlternative6 <: Token
  ]
  (tokens: scala.collection.BufferedIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using c2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4])
  (using ct5: ClassTag[TokenTypeAlternative5])
  (using ct6: ClassTag[TokenTypeAlternative6]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3 | TokenTypeAlternative4 | TokenTypeAlternative5 | TokenTypeAlternative6] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3, TokenTypeAlternative4, TokenTypeAlternative5](tokens).
      orElse(matchToken[TokenTypeAlternative6](tokens))
