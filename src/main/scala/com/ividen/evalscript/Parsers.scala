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
case class LiteralExpression(literal: Literal) extends Expression
case class VariableExpression(variable:Variable) extends Expression
case class PlusExpression(l: Expression,r: Expression) extends Expression
case class MinusExpression(l: Expression,r: Expression) extends Expression
case class DivideExpression(l: Expression,r: Expression) extends Expression
case class MultiplyExpression(l: Expression,r: Expression) extends Expression
case class RemainderExpression(l: Expression,r: Expression) extends Expression
case class LogicalNotExpression(r: Expression) extends Expression
case class BitwiseNotExpression(r: Expression) extends Expression
case class UnaryNegationExpression(r: Expression) extends Expression
case class UnaryPlusExpression(r: Expression) extends Expression
case class PrefixIncrementExpression(r: Expression) extends Expression
case class PrefixDecrementExpression(r: Expression) extends Expression
case class BitwiseLeftShiftExpression(l: Expression,r : Expression) extends Expression
case class BitwiseRightShiftExpression(l: Expression,r : Expression) extends Expression
case class PostfixIncrementExpression(l: Expression) extends Expression
case class PostfixDecrementExpression(l: Expression) extends Expression
case class BitwiseAndExpression(l: Expression, r: Expression) extends Expression
case class BitwiseXorExpression(l: Expression, r: Expression) extends Expression
case class BitwiseOrExpression(l: Expression, r: Expression) extends Expression
case class LogicalAndExpression(l: Expression,r: Expression) extends Expression
case class LogicalOrExpression(l: Expression, r: Expression) extends Expression

trait ExpressionParser extends RegexParsers  with ArithmExpression{
  def expression =  arithm
}

trait ArithmExpression extends RegexParsers with LiteralParser with IdentifierParser{
  type E = Expression

  def arithm: Parser[E] = logicalOrGroup

  def minus: Parser[E => E] = "-" ~> multiplyGroup ^^ { case b => MinusExpression(_, b) }
  def plus: Parser[E => E] = "+" ~> multiplyGroup ^^ { case b => PlusExpression(_, b) }

  def times: Parser[E => E] = "*" ~> factor ^^ { case b => MultiplyExpression(_, b) }
  def divide: Parser[E => E] = "/" ~> factor ^^ { case b => DivideExpression(_, b) }
  def remainder: Parser[E => E] = "%" ~> factor ^^ { case b => RemainderExpression(_, b) }

  def logicalNot: Parser[E] = "!" ~> factor ^^ (LogicalNotExpression(_))
  def bitwiseNot: Parser[E] = "~" ~> factor ^^ (BitwiseNotExpression(_))
  def prefixInc: Parser[E] = "++" ~> factor ^^ (PrefixIncrementExpression(_))
  def prefixDec: Parser[E] = "--" ~> factor ^^ (PrefixDecrementExpression(_))
  def unaryNegate: Parser[E] = "-" ~> factor ^^ (UnaryNegationExpression(_))
  def unaryPlus: Parser[E] = "+" ~> factor ^^ (UnaryNegationExpression(_))

  def bitwiseLeftShift: Parser[E => E] = "<<" ~> addGroup ^^ { case b => BitwiseLeftShiftExpression(_, b) }
  def bitwiseRightShift: Parser[E => E] = ">>" ~> addGroup ^^ { case b => BitwiseRightShiftExpression(_, b) }

  def bitwiseAnd: Parser[E => E] = "&" ~> bitwiseShiftGroup ^^ {case b=>BitwiseAndExpression(_,b)}
  def bitwiseXor: Parser[E => E] = "^" ~> bitwiseAndGroup ^^ {case b=>BitwiseXorExpression(_,b)}
  def bitwiseOr: Parser[E => E] = "|" ~> bitwiseXorGroup ^^ {case b=>BitwiseOrExpression(_,b)}
  def logicalAnd: Parser[E => E] = "&&" ~> bitwiseOrGroup ^^ {case b=>LogicalAndExpression(_,b)}
  def logicalOr: Parser[E => E] = "||" ~> logicalAndGroup ^^ {case b=>LogicalOrExpression(_,b)}


  private def factor: Parser[E] = scriptLiteral ^^ (LiteralExpression(_)) | "(" ~> arithm <~ ")" | unaryGroup
  private def foldExpression(exp: (E ~ List[(E) => E])) = exp._2.foldLeft(exp._1)((x, f) => f(x))

  private def bitwiseAndGroup = operationPrecedence(bitwiseShiftGroup,bitwiseAnd)
  private def bitwiseXorGroup = operationPrecedence(bitwiseAndGroup,bitwiseXor)
  private def bitwiseOrGroup = operationPrecedence(bitwiseXorGroup,bitwiseOr)
  private def logicalAndGroup = operationPrecedence(bitwiseOrGroup,logicalAnd)
  private def logicalOrGroup = operationPrecedence(logicalAndGroup,logicalOr)
  private def unaryGroup = logicalNot | bitwiseNot | prefixInc | prefixDec | unaryNegate | unaryPlus
  private def multiplyGroup = operationPrecedence(factor,times | divide | remainder)
  private def addGroup = operationPrecedence(multiplyGroup, plus | minus)
  private def bitwiseShiftGroup = operationPrecedence(addGroup,bitwiseLeftShift | bitwiseRightShift)
  private def operationPrecedence (before: Parser[E], func : Parser[E=>E]) = before ~ rep(func) ^^ foldExpression
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

sealed trait Literal extends ExpressionElement {
  type T

  def value: T

  def +(l: Literal) : Literal = unsupportedOperation
  def -(l: Literal) : Literal = unsupportedOperation
  def *(l: Literal) : Literal = unsupportedOperation
  def /(l: Literal) : Literal = unsupportedOperation
  def %(l: Literal) : Literal = unsupportedOperation
  def <<(l: Literal) : Literal =unsupportedOperation
  def >>(l: Literal) : Literal =unsupportedOperation
  def unary_! : Literal = unsupportedOperation
  def unary_~ : Literal = unsupportedOperation
  def unary_- : Literal = unsupportedOperation
  def unary_+ : Literal = unsupportedOperation
  def &(l: Literal) : Literal = unsupportedOperation
  def ^(l: Literal) : Literal = unsupportedOperation
  def |(l: Literal) : Literal = unsupportedOperation
  def &&(l: Literal) : Literal = unsupportedOperation
  def ||(l: Literal) : Literal = unsupportedOperation

  def toBooleanLiteral: BooleanLiteral = unsupportedOperation
  def toStringLiteral : StringLiteral= unsupportedOperation
  def toDecimalLiteral: DecimalLiteral= unsupportedOperation

  private def unsupportedOperation[U]: U = throw new UnsupportedOperationException("Operation is not supported!")
}

object NullLiteral extends Literal{
  type T = Unit

  override def value: Unit = {}
}
case class BooleanLiteral(value: Boolean) extends Literal{
  type T = Boolean

  override def unary_! : Literal = BooleanLiteral(!value)
  override def unary_~ : Literal = BooleanLiteral(!value)
  override def &(l: Literal) : Literal = BooleanLiteral(value & l.toBooleanLiteral.value)
  override def ^(l: Literal) : Literal = BooleanLiteral(value ^ l.toBooleanLiteral.value)
  override def |(l: Literal) : Literal = BooleanLiteral(value | l.toBooleanLiteral.value)
  override def &&(l: Literal) : Literal = BooleanLiteral(value && l.toBooleanLiteral.value)
  override def ||(l: Literal) : Literal = BooleanLiteral(value || l.toBooleanLiteral.value)

  override def toBooleanLiteral: BooleanLiteral = this
  override def toStringLiteral: StringLiteral = StringLiteral(value.toString)
  override def toDecimalLiteral: DecimalLiteral = DecimalLiteral(BigDecimal(if(value) 1 else 0))
}

case class DecimalLiteral(value: BigDecimal) extends Literal{
  type T = BigDecimal

  override def +(l: Literal): Literal = DecimalLiteral(this.value + l.toDecimalLiteral.value)
  override def -(l: Literal): Literal = DecimalLiteral(this.value - l.toDecimalLiteral.value)
  override def *(l: Literal): Literal = DecimalLiteral(this.value * l.toDecimalLiteral.value)
  override def /(l: Literal): Literal = DecimalLiteral(this.value / l.toDecimalLiteral.value)
  override def %(l: Literal): Literal = DecimalLiteral(this.value % l.toDecimalLiteral.value)
  override def <<(l: Literal): Literal = DecimalLiteral(BigDecimal(this.value.toBigInt() << l.toDecimalLiteral.value.toInt))
  override def >>(l: Literal): Literal = DecimalLiteral(BigDecimal(this.value.toBigInt() >> l.toDecimalLiteral.value.toInt))
  override def unary_! : Literal = BooleanLiteral(if (value == 0) true else false)
  override def unary_~ : Literal = DecimalLiteral(BigDecimal(~value.toBigInt()))
  override def unary_- : Literal = DecimalLiteral(-value)
  override def unary_+  : Literal = this
  override def &(l: Literal) : Literal = DecimalLiteral(BigDecimal(value.toBigInt() & l.toDecimalLiteral.value.toBigInt()))
  override def |(l: Literal) : Literal = DecimalLiteral(BigDecimal(value.toBigInt() | l.toDecimalLiteral.value.toBigInt()))
  override def ^(l: Literal) : Literal = DecimalLiteral(BigDecimal(value.toBigInt() ^ l.toDecimalLiteral.value.toBigInt()))
  override def toBooleanLiteral: BooleanLiteral = BooleanLiteral(if (value == 0) false else true)
  override def toStringLiteral: StringLiteral = StringLiteral(value.toString)
  override def toDecimalLiteral: DecimalLiteral = this
}
case class StringLiteral(value: String) extends Literal{
  type T = String

  override def +(l: Literal): Literal = StringLiteral(this.value + l.toStringLiteral.value)
  override def *(l: Literal): Literal = StringLiteral((1 to l.toDecimalLiteral.value.toInt).foldLeft[String](this.value)((x1,x2) => x1 + value))
  override def toBooleanLiteral: BooleanLiteral = BooleanLiteral(if (value == 0) false else true)
  override def toStringLiteral: StringLiteral = this
  override def toDecimalLiteral: DecimalLiteral = DecimalLiteral(BigDecimal(value))
}

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

object Main extends EvalScriptParser {

  def main(args: Array[String]) {
    //    val v2 = """true false null 10 20 30 10.1 0x1987FA 0x30 1000000000000000000000000000000"""
    val v2 ="100 - (true && false)"

    //    val v2 = """0x1987FA"""
    //    val v3 = """ 'Test "1"' "Test '2'""""

    val all  = parseAll(program, v2)

    val e1: Expression = null
    val e2: Expression = null

    println(all)
    Interpreter.process(all.get)
    //    println(parse(literal, v2))
    //    println(parse(literal, v3))
  }
}


