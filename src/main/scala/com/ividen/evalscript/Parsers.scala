package com.ividen.evalscript

import org.apache.commons.lang3.StringEscapeUtils
import scala.util.parsing.combinator._

trait EvalScriptParser extends IfElseParser with RepeatParser with StatementParser with ArithmParser with LiteralParser with IdentifierParser with AssignmentParser{
  def script : Parser[Script] = statemetList ^^ (Script(_))
}

trait IfElseParser extends RegexParsers{ self: StatementParser with ArithmParser =>
  def if_else: Parser[ScriptElement] = if_ ~ rep(_else) ^^ { case i ~ e => `if else`(i, e) }
  private def if_ = "if" ~> (condition ~ statement) ^^ { case c ~ s => `if`(c, `{}`(Seq(s))) }
  private def _else = ("else" ~> (condition.? ~ statement)) ^^ { case c ~ s => `else`(c, `{}`(Seq(s))) }
  private def condition: Parser[Expression] = "(" ~> arithm <~ ")"
}

trait RepeatParser extends RegexParsers{ self: StatementParser with ArithmParser =>
  def for_ : Parser[ScriptElement] = forBegin ~> arithm ~ ";" ~ arithm ~ ":" ~ arithm ~ ")" ~ statement ^^ { case init ~ _ ~ check ~ _ ~ postfix ~ _ ~ statement => `{}`(Seq(init, `while do`(check, `{}`(Seq(statement, postfix))))) }
  def doWhile: Parser[ScriptElement] = doStatement ~ "while" ~ condition ^^ { case s ~ _ ~ c => `do while`(c, `{}`(Seq(s))) }
  def whileDo: Parser[ScriptElement] = "while" ~> condition ~ doStatement ^^ { case c ~ s => `while do`(c, `{}`(Seq(s))) }

  def repeat = for_ | doWhile | whileDo
  private def doStatement = "do" ~> statement
  private def condition: Parser[Expression] = "(" ~> arithm <~ ")"
  private def forBegin = "for" ~ "("
}

trait StatementParser extends RegexParsers { self: ArithmParser with AssignmentParser with IfElseParser with RepeatParser=>
  def statemetList = statements ^^ (`{}`(_))
  def statement: Parser[ScriptElement] =  repeat | if_else | assignments | arithm | block
  def statements =  statement*
  def block: Parser[ScriptElement]= ("{" ~> statements )<~"}" ^^ (`{}`(_))
}

trait ArithmParser extends RegexParsers {self: LiteralParser with IdentifierParser =>
  type E = Expression

  def arithm: Parser[E] = logicalOrGroup

  def minus: Parser[E => E] = ("-" ~> multiplyGroup) ^^ { case b => `:-`(_, b) }
  def plus: Parser[E => E] = ("+" ~> multiplyGroup) ^^ { case b => `:+`(_, b) }

  def times: Parser[E => E] = "*" ~> factor ^^ { case b => `*`(_, b) }
  def divide: Parser[E => E] = "/" ~> factor ^^ { case b => `/`(_, b) }
  def remainder: Parser[E => E] = "%" ~> factor ^^ { case b => `%`(_, b) }

  def postfixInc: Parser[E] = variable <~ "++" ^^ `:++`
  def postfixDec: Parser[E] = variable <~ "--" ^^ `:--`

  def logicalNot: Parser[E] = "!" ~> factor ^^ `!:`
  def bitwiseNot: Parser[E] = "~" ~> factor ^^ `~:`
  def prefixInc: Parser[E] = "++" ~> variable ^^ `++:`
  def prefixDec: Parser[E] = "--" ~> variable ^^ `--:`
  def unaryNegate: Parser[E] = "-" ~> factor ^^ `-:`
  def unaryPlus: Parser[E] = "+" ~> factor ^^ `+:`

  def bitwiseLeftShift: Parser[E => E] = "<<" ~> addGroup ^^ { case b => `>>`(_, b) }
  def bitwiseRightShift: Parser[E => E] = ">>" ~> addGroup ^^ { case b => `<<`(_, b) }

  def bitwiseAnd: Parser[E => E] = "&" ~> bitwiseShiftGroup ^^ { case b => `&`(_, b) }
  def bitwiseXor: Parser[E => E] = "^" ~> bitwiseAndGroup ^^ { case b => `^`(_, b) }
  def bitwiseOr: Parser[E => E] = "|" ~> bitwiseXorGroup ^^ { case b => `|`(_, b) }
  def logicalAnd: Parser[E => E] = "&&" ~> bitwiseOrGroup ^^ { case b => `&&`(_, b) }
  def logicalOr: Parser[E => E] = "||" ~> logicalAndGroup ^^ { case b => `||`(_, b) }


  def isEq: Parser[E => E] = "==" ~> arithm ^^ { case b => `:==`(_, b) }
  def isNotEq: Parser[E => E] = "!=" ~> arithm ^^ { case b => `:!=`(_, b) }
  def lessThen: Parser[E => E] = "<" ~> arithm ^^ { case b => `<`(_, b) }
  def lessThenOrEq: Parser[E => E] = "<=" ~> arithm ^^ { case b => `<=`(_, b) }
  def greaterThen: Parser[E => E] = ">" ~> arithm ^^ { case b => `>`(_, b) }
  def greaterThenOrEq: Parser[E => E] = ">=" ~> arithm ^^ { case b => `>`(_, b) }


  private def factor: Parser[E] = literalExpression | postfixGroup | variableExpression  | "(" ~> arithm <~ ")"  | unaryGroup
  private def literalExpression: Parser[E] = scriptLiteral ^^ LiteralExpression
  private def variableExpression: Parser[E] = variable ^^ GerVar
  private def foldExpression(exp: (E ~ List[(E) => E])) = exp._2.foldLeft(exp._1)((x, f) => f(x))

  private def nonStrictConditionGroup = operationPrecedence(bitwiseShiftGroup,lessThen | lessThenOrEq | greaterThen | greaterThenOrEq)
  private def strictConditionGroup = operationPrecedence(nonStrictConditionGroup,lessThen | lessThenOrEq | greaterThen | greaterThenOrEq)
  private def postfixGroup = postfixInc | postfixDec
  private def bitwiseAndGroup = operationPrecedence(strictConditionGroup,bitwiseAnd)
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

trait AssignmentParser extends RegexParsers { self: ArithmParser with IdentifierParser =>
  implicit def variableToExpression(v: Variable):Expression = GerVar(v)

  def assignments = declareVars | assign | assignPlus |assignMinus |assignTimes |assignDivide |assignRemainder |
    assignLogicalNot|assignBitwiseNot | assignBitwiseRightShift |
    assignBitwiseAnd |assignBitwiseXor | assignBitwiseOr | assignLogicalAnd | assignLogicalOr

  def declareVars : Parser[DeclareVars] = "var" ~> repsep(newVar,",".r)<~"[' '\n\r;]*".r ^^ (DeclareVars(_))
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

trait IdentifierParser extends RegexParsers{
  type V = Variable
  val idName = "[a-zA-Z_]+([a-zA-Z0-9_])*".r
  def variable: Parser[V] = globalVariable | localVariable
  def globalVariable: Parser[V] = "$" ~> idName ^^ (GlobalVairable(_))
  def localVariable: Parser[V] = idName ^^ (LocalVariable(_))
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
      |
      |
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
    Interpreter.process(all.get,new GlobalContext())
    //    println(parse(literal, v2))
    //    println(parse(literal, v3))
  }
}


