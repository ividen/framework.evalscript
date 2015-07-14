package com.ividen.evalscript

import org.apache.commons.lang3.StringEscapeUtils

import scala.util.parsing.combinator._

trait EvalScriptParser extends ControlFlowParser with ExpressionParser
trait ControlFlowParser extends IfElseParser with RepeatParser
trait IfElseParser extends RegexParsers
trait RepeatParser extends RegexParsers


trait Expression
case class LiteralExpression(literal: Literal) extends Expression
case class PlusExpression(l: Expression,r: Expression) extends Expression
case class MinusExpression(l: Expression,r: Expression) extends Expression
case class DivideExpression(l: Expression,r: Expression) extends Expression
case class MultiplyExpression(l: Expression,r: Expression) extends Expression
case class ModExpression(l: Expression,r: Expression) extends Expression

trait ExpressionParser extends RegexParsers with LiteralParser {


  def singleExpression: Parser[Expression] = literalExpression | addExpression | minusExpression | multExpression | devideExpression | modExpression | parenthExpression

  def literalExpression= scriptLiteral ^^ (LiteralExpression(_))
  def addExpression: Parser[Expression] = singleExpression ~ '+' ~ singleExpression ^^ (x => PlusExpression(x._1._1,x._2))
  def minusExpression: Parser[Expression] = singleExpression ~ '-' ~ singleExpression ^^ (x => MinusExpression(x._1._1,x._2))
  def multExpression: Parser[Expression] = singleExpression ~ '*' ~ singleExpression ^^ (x => MultiplyExpression(x._1._1,x._2))
  def devideExpression: Parser[Expression] = singleExpression ~ '/' ~ singleExpression ^^ (x => DivideExpression(x._1._1,x._2))
  def modExpression: Parser[Expression] = singleExpression ~ '%' ~ singleExpression ^^ (x => ModExpression(x._1._1,x._2))
  def parenthExpression: Parser[Expression] = "(" ~> singleExpression <~ ")"
}

trait ArithmExpression extends RegexParsers with LiteralParser{
  def arithm: Parser[Expression] = term ~ rep(plus | minus) ^^ foldExpression
  def minus: Parser[Expression => Expression] = "-" ~> factor ^^ { case b => MinusExpression(_, b) }
  def plus: Parser[Expression => Expression] = "+" ~> factor ^^ { case b => PlusExpression(_, b) }
  def term: Parser[Expression] = factor ~ rep(times | divide) ^^ foldExpression
  def times: Parser[Expression => Expression] = "*" ~> factor ^^ { case b => MultiplyExpression(_, b) }
  def divide: Parser[Expression => Expression] = "/" ~> factor ^^ { case b => DivideExpression(_, b) }
  private def factor: Parser[Expression] = scriptLiteral ^^ (LiteralExpression(_)) | "(" ~> arithm <~ ")"
  private def foldExpression(exp: (Expression ~ List[(Expression) => Expression])) = exp._2.foldLeft(exp._1)((x, f) => f(x))
}

sealed trait Literal
object NullLiteral extends Literal
case class BooleanLiteral(value: Boolean) extends Literal
case class DecimalLiteral(value: BigDecimal) extends Literal
case class StringLiteral(value: String) extends Literal

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
    val v2 ="10 * 20 + 10 * 20-30"

    //    val v2 = """0x1987FA"""
    //    val v3 = """ 'Test "1"' "Test '2'""""


    println(parseAll(arithm, v2))
    //    println(parse(literal, v2))
    //    println(parse(literal, v3))
  }
}
