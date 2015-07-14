package com.ividen.evalscript

import org.apache.commons.lang3.StringEscapeUtils
import scala.util.parsing.combinator._

trait ProgramElement

trait EvalScriptParser extends ControlFlowParser with ExpressionParser{
  type PE = ProgramElement
  def program : Parser[List[PE]] = rep(expression)
}
trait ControlFlowParser extends IfElseParser with RepeatParser
trait IfElseParser extends RegexParsers
trait RepeatParser extends RegexParsers

sealed trait Expression extends ProgramElement
case class LiteralExpression(literal: Literal[_]) extends Expression
case class VariableExpression(variable:Variable) extends Expression
case class PlusExpression(l: Expression,r: Expression) extends Expression
case class MinusExpression(l: Expression,r: Expression) extends Expression
case class DivideExpression(l: Expression,r: Expression) extends Expression
case class MultiplyExpression(l: Expression,r: Expression) extends Expression
case class RemainderExpression(l: Expression,r: Expression) extends Expression
case class LogicalNotExpression(r: Expression) extends Expression
case class BitwiseNotExpression(r: Expression) extends Expression
case class BitwiseLeftShiftExpression(l: Expression,r : Expression) extends Expression
case class BitwiseRightShiftExpression(l: Expression,r : Expression) extends Expression
case class BitwiseUnsignedRightShiftExpression(l: Expression,r : Expression) extends Expression

trait ExpressionParser extends RegexParsers  with ArithmExpression{
  def expression =  arithm
}

trait ArithmExpression extends RegexParsers with LiteralParser with IdentifierParser{
  type E = Expression

  def arithm: Parser[E] = multPrecedence ~ rep(plus | minus) ^^ foldExpression

  def minus: Parser[E => E] = "-" ~> multPrecedence ^^ { case b => MinusExpression(_, b) }
  def plus: Parser[E => E] = "+" ~> multPrecedence ^^ { case b => PlusExpression(_, b) }

  def times: Parser[E => E] = "*" ~> factor ^^ { case b => MultiplyExpression(_, b) }
  def divide: Parser[E => E] = "/" ~> factor ^^ { case b => DivideExpression(_, b) }
  def remainder: Parser[E => E] = "%" ~> factor ^^ { case b => RemainderExpression(_, b) }

  def logicalNot: Parser[E] = "!" ~> factor ^^ (LogicalNotExpression(_))
  def bitwiseNot: Parser[E] = "~" ~> factor ^^ (BitwiseNotExpression(_))

  def bitwiseLeftShift: Parser[E => E] = "<<" ~> factor ^^ { case b => BitwiseLeftShiftExpression(_, b) }
  def bitwiseRightShift: Parser[E => E] = ">>" ~> factor ^^ { case b => BitwiseRightShiftExpression(_, b) }
  def bitwiseUnsignedRightShift: Parser[E => E] = ">>>" ~> factor ^^ { case b => BitwiseUnsignedRightShiftExpression(_, b) }

  private def bitwiseShiftPrecendence = addPrecendence ~ rep(bitwiseLeftShift | bitwiseRightShift | bitwiseUnsignedRightShift) ^^ foldExpression
  private def addPrecendence = multPrecedence ~ rep(plus | minus) ^^ foldExpression
  private def logicalUnaryPrecendence = factor ~ rep(logicalNot | bitwiseNot) ^^ foldExpression
  private def multPrecedence: Parser[E] = logicalUnaryPrecendence ~ rep(times | divide | remainder) ^^ foldExpression
  private def factor: Parser[E] = scriptLiteral ^^ (LiteralExpression(_)) | "(" ~> arithm <~ ")"
  private def foldExpression(exp: (E ~ List[(E) => E])) = exp._2.foldLeft(exp._1)((x, f) => f(x))
}


sealed trait ExpressionElement extends ProgramElement

sealed trait Variable extends ExpressionElement{
  def name : String
}
case class LocalVariable(name: String) extends Variable
case class GlobalVairable(name: String) extends Variable

trait IdentifierParser extends RegexParsers{
  type V = Variable
  val idName = "[a-zA-Z_]+([a-zA-Z0-9_])*".r
  def variable: Parser[V] = globalVariable | localVariable
  def globalVariable: Parser[V] = "$" ~> idName ^^ (GlobalVairable(_))
  def localVariable: Parser[V] = idName ^^ (LocalVariable(_))
}

sealed trait Literal[T] extends ExpressionElement{
  def value: T
}

object NullLiteral extends Literal[Unit]{
  override def value: Unit = {}
}
case class BooleanLiteral(value: Boolean) extends Literal[Boolean]
case class DecimalLiteral(value: BigDecimal) extends Literal[BigDecimal]
case class StringLiteral(value: String) extends Literal[String]

trait LiteralParser extends RegexParsers {
  override def skipWhitespace: Boolean = true
  def nullLiteral =  "null" ^^ (_ => NullLiteral)
  def booleanLiteral = ("true" | "false") ^^ toBooleanLiteral
  def numericLiteral = hexIntegerLiteral | decimalLiteral
  def stringLiteral = doubleQuoteStringLiteral | singleQuoteStringLiteral
  def scriptLiteral = nullLiteral | booleanLiteral | numericLiteral | stringLiteral
  def scriptLiterals = rep(scriptLiteral)

  private def decimalLiteral= """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ toDecimalLiteral
  private def hexIntegerLiteral = "0(x|X)[0-9a-fA-F]+".r ^^ toHexDecimalLiteral
  private def doubleQuoteStringLiteral = "\"" ~> "[^\"\n\t]*".r <~ "\"" ^^ toStringLiteral
  private def singleQuoteStringLiteral = "'" ~> "[^'\n\t]*".r <~ "'" ^^ toStringLiteral
  private def toDecimalLiteral(x: String) = DecimalLiteral(BigDecimal(x))
  private def toHexDecimalLiteral(x: String) = DecimalLiteral(BigDecimal(Integer.parseInt(x.substring(2), 16)))
  private def toStringLiteral(x: String) = StringLiteral(StringEscapeUtils.unescapeJava(x))
  private def toBooleanLiteral(x: String) = BooleanLiteral(x.toBoolean)
}

object Main extends ArithmExpression {

  def main(args: Array[String]) {
    //    val v2 = """true false null 10 20 30 10.1 0x1987FA 0x30 1000000000000000000000000000000"""
    val v2 ="$dsfsdf"

    //    val v2 = """0x1987FA"""
    //    val v3 = """ 'Test "1"' "Test '2'""""


    println(parseAll(variable, v2))
    //    println(parse(literal, v2))
    //    println(parse(literal, v3))
  }
}

