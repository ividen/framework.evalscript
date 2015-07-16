package com.ividen.evalscript

import com.ividen.evalscript.DeclareVars
import org.apache.commons.lang3.StringEscapeUtils
import scala.util.parsing.combinator._

trait ScriptElement
case class Script(items: Seq[ScriptElement])

trait EvalScriptParser extends ControlFlowParser with ExpressionParser{
  def script : Parser[Script] = rep(expression) ^^ (Script(_))
}
trait ControlFlowParser extends IfElseParser with RepeatParser
trait IfElseParser extends RegexParsers
trait RepeatParser extends RegexParsers

case class DeclareVars(items: Seq[`=`]) extends ScriptElement

case class `=`(l:Variable,r: Expression) extends ScriptElement

sealed trait Expression extends ScriptElement
case class LiteralExpression(literal: Literal) extends Expression
case class GerVar(variable:Variable) extends Expression


case class `:+`(l: Expression,r: Expression) extends Expression
case class `:-`(l: Expression,r: Expression) extends Expression
case class `/`(l: Expression,r: Expression) extends Expression
case class `*`(l: Expression,r: Expression) extends Expression
case class `%`(l: Expression,r: Expression) extends Expression
case class `!:`(r: Expression) extends Expression
case class `~:`(r: Expression) extends Expression
case class `-:`(r: Expression) extends Expression
case class `+:`(r: Expression) extends Expression
case class `++:`(r: Expression) extends Expression
case class `--:`(r: Expression) extends Expression
case class `<<`(l: Expression,r : Expression) extends Expression
case class `>>`(l: Expression,r : Expression) extends Expression
case class `++`(l: Expression) extends Expression
case class `--`(l: Expression) extends Expression
case class `&`(l: Expression, r: Expression) extends Expression
case class `^`(l: Expression, r: Expression) extends Expression
case class `|`(l: Expression, r: Expression) extends Expression
case class `&&`(l: Expression,r: Expression) extends Expression
case class `||`(l: Expression, r: Expression) extends Expression

trait ExpressionParser extends RegexParsers  with ArithmParser with AssignmentParser{
  def expression =  assignments | arithm
}

trait ArithmParser extends RegexParsers with LiteralParser with IdentifierParser{
  type E = Expression

  def arithm: Parser[E] = logicalOrGroup

  def minus: Parser[E => E] = ("-" ~> multiplyGroup) ^^ { case b => `:-`(_, b) }
  def plus: Parser[E => E] = ("+" ~> multiplyGroup) ^^ { case b => `:+`(_, b) }

  def times: Parser[E => E] = "*" ~> factor ^^ { case b => `*`(_, b) }
  def divide: Parser[E => E] = "/" ~> factor ^^ { case b => `/`(_, b) }
  def remainder: Parser[E => E] = "%" ~> factor ^^ { case b => `%`(_, b) }

  def logicalNot: Parser[E] = "!" ~> factor ^^ `!:`
  def bitwiseNot: Parser[E] = "~" ~> factor ^^ `~:`
  def prefixInc: Parser[E] = "++" ~> factor ^^ `++:`
  def prefixDec: Parser[E] = "--" ~> factor ^^ `--:`
  def unaryNegate: Parser[E] = "-" ~> factor ^^ `-:`
  def unaryPlus: Parser[E] = "+" ~> factor ^^ `+:`

  def bitwiseLeftShift: Parser[E => E] = "<<" ~> addGroup ^^ { case b => `>>`(_, b) }
  def bitwiseRightShift: Parser[E => E] = ">>" ~> addGroup ^^ { case b => `<<`(_, b) }

  def bitwiseAnd: Parser[E => E] = "&" ~> bitwiseShiftGroup ^^ { case b => `&`(_, b) }
  def bitwiseXor: Parser[E => E] = "^" ~> bitwiseAndGroup ^^ { case b => `^`(_, b) }
  def bitwiseOr: Parser[E => E] = "|" ~> bitwiseXorGroup ^^ { case b => `|`(_, b) }
  def logicalAnd: Parser[E => E] = "&&" ~> bitwiseOrGroup ^^ { case b => `&&`(_, b) }
  def logicalOr: Parser[E => E] = "||" ~> logicalAndGroup ^^ { case b => `||`(_, b) }

  private def factor: Parser[E] = literalExpression | variableExpression  | "(" ~> arithm <~ ")" | unaryGroup
  private def literalExpression: Parser[E] = scriptLiteral ^^ LiteralExpression
  private def variableExpression: Parser[E] = variable ^^ GerVar
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

trait AssignmentParser extends ArithmParser{
  implicit def variableToExpression(v: Variable):Expression = GerVar(v)

  def assignments = declareVars | assign | assignPlus |assignMinus |assignTimes |assignDivide |assignRemainder |
    assignLogicalNot|assignBitwiseNot | assignBitwiseRightShift |
    assignBitwiseAnd |assignBitwiseXor | assignBitwiseOr | assignLogicalAnd | assignLogicalOr

  def declareVars : Parser[DeclareVars] = "var" ~> repsep(newVar,",".r) ^^ (DeclareVars(_))
  def assign: Parser[`=`] = variable ~ "=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, a) }
  def assignPlus: Parser[`=`] = variable ~ "+=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `:+`(v, a)) }
  def assignMinus: Parser[`=`] = variable ~ "-=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `:-`(v, a)) }
  def assignTimes: Parser[`=`] = variable ~ "*=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `*`(v, a)) }
  def assignDivide: Parser[`=`] = variable ~ "/=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `/`(v, a)) }
  def assignRemainder: Parser[`=`] = variable ~ "%=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `%`(v, a)) }
  def assignLogicalNot: Parser[`=`] = variable ~ "!=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `!:`(a)) }
  def assignBitwiseNot: Parser[`=`] = variable ~ "~=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `~:`(a)) }
  def assignBitwiseLeftShift: Parser[`=`] = variable ~ "<<=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `>>`(v, a)) }
  def assignBitwiseRightShift: Parser[`=`] = variable ~ ">>=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `<<`(v, a)) }
  def assignBitwiseAnd: Parser[`=`] = variable ~ "&=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `&`(v, a)) }
  def assignBitwiseXor: Parser[`=`] = variable ~ "^=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `^`(v, a)) }
  def assignBitwiseOr: Parser[`=`] = variable ~ "|=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `|`(v, a)) }
  def assignLogicalAnd: Parser[`=`] = variable ~ "&&=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `&&`(v, a)) }
  def assignLogicalOr: Parser[`=`] = variable ~ "||=" ~ arithm ^^ { case v ~ _ ~ a => `=`(v, `||`(v, a)) }
  private def newVar: Parser[`=`] = assign | nullVar
  private def nullVar: Parser[`=`] = variable ^^ { case v => `=`(v, LiteralExpression(NullLiteral)) }

}

sealed trait ExpressionElement extends ScriptElement

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
    val v2 ="10+10*10+20*30 - (10-1)/(1+2)"

    """
      |val v1 = 1, v2 = 1
      |
      |if($purchaseCount <= 10){
      |   v1 =
      |   $multiplier = 10
      |}elsed
      |
      |
    """.stripMargin

    //    val v2 = """0x1987FA"""
    //    val v3 = """ 'Test "1"' "Test '2'""""

    val all  = parseAll(script, v2)

    val e1: Expression = null
    val e2: Expression = null

    println(all)
    val ctx = ExecutionContext()
    Interpreter.process(all.get,ctx)
    //    println(parse(literal, v2))
    //    println(parse(literal, v3))
  }
}


