package com.ividen.evalscript

import org.apache.commons.lang3.StringEscapeUtils
import scala.util.parsing.combinator._
import scala.reflect.runtime._

object EvalScriptParser extends IfElseParser with RepeatParser with SwitchParser  with BreakParser
                           with StatementParser with ExpressionParser with LiteralParser with IdentifierParser
                           with AssignmentParser with KeywordParser with FunctionParser{
  def script : Parser[Script] = statementList ^^ (Script(_))

  def load(s: String) : Script = parseAll(script,s).get
}

trait FunctionParser extends RegexParsers{self: ExpressionParser with IdentifierParser =>
  // todo aguzanov how to return parser error
  def function: Parser[Expression] = idName ~ arguments ^^ { case n ~ a => if (FunctionInvoker.hasMethod(n)) `call`(n, a) else throw new IllegalStateException(s"Unknown function $n with arguments: $a") }
  private def arguments = "("~> repsep(expression,",") <~")"
}

trait IfElseParser extends RegexParsers{ self: StatementParser with ExpressionParser =>
  def if_else: Parser[ScriptElement] = if_ ~ rep(_else) ^^ { case i ~ e => `if else`(i, e) }
  private def if_ = "if" ~> (condition ~ statement) ^^ { case c ~ s => `if`(c, `{}`(Seq(s))) }
  private def _else = ("else" ~> (condition.? ~ statement)) ^^ { case c ~ s => `else`(c, `{}`(Seq(s))) }
  private def condition: Parser[Expression] = "(" ~> expression <~ ")"
}

trait SwitchParser extends RegexParsers{self: StatementParser with ExpressionParser with LiteralParser with KeywordParser =>
  def switch_case = switch_ ~ rep(case_)~ default_ .? <~ "}" ^^ { case c~cases~d => `switch`(c, cases, d.flatMap(x=>x)) }
  private def case_ = case_literal ~ statementList.? ^^ { case l ~ b => `case`(l, b) }
  private def case_literal =  caseKeyword ~> expression <~ ":"
  private def switch_ = switchKeyword ~> condition <~ "{"
  private def default_ = defaultKeyword ~> ":" ~> statementList.?
  private def condition: Parser[Expression] = "(" ~> expression <~ ")"
}

trait RepeatParser extends RegexParsers{ self: StatementParser with ExpressionParser with AssignmentParser with KeywordParser =>
  def repeat = for_ | doWhile | whileDo
  def for_ : Parser[ScriptElement] = forKeyword~ "(" ~ assignments ~ ";" ~ expression ~ ";" ~ expression ~ ")" ~ statement ^^ { case  _ ~ _~  init ~ _ ~ check ~ _ ~ postfix ~ _ ~ statement => `{}`(Seq(init, `while do`(check, `{}`(Seq(statement)),Some(postfix))))}
  def doWhile: Parser[ScriptElement] = doStatement ~ whileKeyword~ condition ^^ { case s ~ _ ~ c => `do while`(c, `{}`(Seq(s))) }
  def whileDo: Parser[ScriptElement] = whileKeyword~> condition ~ statement ^^ { case c ~ s => `while do`(c, `{}`(Seq(s))) }
  private def doStatement = doKeyword ~> statement
  private def condition: Parser[Expression] = "(" ~> expression <~ ")"
}

trait BreakParser extends RegexParsers{self: KeywordParser =>
  def break: Parser[`break`] = breakKeyword^^ {_ => new `break`}
  def continue: Parser[`continue`] = continueKeyword ^^ {_ => new `continue`}
}

trait StatementParser extends RegexParsers { self: ExpressionParser with AssignmentParser with IfElseParser with RepeatParser with SwitchParser with BreakParser=>
  def statementList = statements ^^ (`{}`(_))
  def statement: Parser[ScriptElement] =  break | continue | repeat | switch_case | if_else | assignments | expression | block
  def statements =  statement*
  def block: Parser[ScriptElement]= ("{" ~> statements )<~"}" ^^ (`{}`(_))
}

trait ExpressionParser extends RegexParsers {self: LiteralParser with IdentifierParser with FunctionParser=>
  type E = Expression

  def expression: Parser[E] = logicalOrGroup

  def postfixInc: Parser[E] = variable <~ "++" ^^ `:++`
  def postfixDec: Parser[E] = variable <~ "--" ^^ `:--`

  def index: Parser[E=>E] =  "[" ~ factor ~ "]" ^^ { case  _ ~ r ~ _  => `[]`(_, r) }

  def logicalNot: Parser[E] = "!" ~> indexGroup ^^ `!:`
  def bitwiseNot: Parser[E] = "~" ~> indexGroup ^^ `~:`
  def prefixInc: Parser[E] = "++" ~> variable ^^ `++:`
  def prefixDec: Parser[E] = "--" ~> variable ^^ `--:`
  def unaryNegate: Parser[E] = "-" ~> indexGroup ^^ `-:`
  def unaryPlus: Parser[E] = "+" ~> indexGroup ^^ `+:`

  def times: Parser[E => E] = "*" ~> unaryGroup ^^ { case b => `*`(_, b) }
  def divide: Parser[E => E] = "/" ~> unaryGroup ^^ { case b => `/`(_, b) }
  def remainder: Parser[E => E] = "%" ~> unaryGroup ^^ { case b => `%`(_, b) }

  def minus: Parser[E => E] = ("-" ~> multiplyGroup) ^^ { case b => `:-`(_, b) }
  def plus: Parser[E => E] = ("+" ~> multiplyGroup) ^^ { case b => `:+`(_, b) }

  def bitwiseLeftShift: Parser[E => E] = "<<" ~> addGroup ^^ { case b => `>>`(_, b) }
  def bitwiseRightShift: Parser[E => E] = ">>" ~> addGroup ^^ { case b => `<<`(_, b) }

  def lessThen: Parser[E => E] = "<" ~> bitwiseShiftGroup ^^ { case b => `<`(_, b) }
  def lessThenOrEq: Parser[E => E] = "<=" ~> bitwiseShiftGroup ^^ { case b => `<=`(_, b) }
  def greaterThen: Parser[E => E] = ">" ~> bitwiseShiftGroup ^^ { case b => `>`(_, b) }
  def greaterThenOrEq: Parser[E => E] = ">=" ~> bitwiseShiftGroup ^^ { case b => `>=`(_, b) }

  def isEq: Parser[E => E] = "==" ~> nonStrictConditionGroup ^^ { case b => `:==`(_, b) }
  def isNotEq: Parser[E => E] = "!=" ~> nonStrictConditionGroup ^^ { case b => `:!=`(_, b) }

  def bitwiseAnd: Parser[E => E] = "&" ~> strictConditionGroup ^^ { case b => `&`(_, b) }
  def bitwiseXor: Parser[E => E] = "^" ~> bitwiseAndGroup ^^ { case b => `^`(_, b) }
  def bitwiseOr: Parser[E => E] = "|" ~> bitwiseXorGroup ^^ { case b => `|`(_, b) }
  def logicalAnd: Parser[E => E] = "&&" ~> bitwiseOrGroup ^^ { case b => `&&`(_, b) }
  def logicalOr: Parser[E => E] = "||" ~> logicalAndGroup ^^ { case b => `||`(_, b) }

  private def factor: Parser[E] = function | literalExpression | variableExpression | "(" ~> expression <~ ")"
  private def literalExpression: Parser[E] = scriptLiteral ^^ LiteralExpression
  private def variableExpression: Parser[E] = variable ^^ GerVar
  private def foldExpression(exp: (E ~ List[(E) => E])) = exp._2.foldLeft(exp._1)((x, f) => f(x))
  private def indexGroup = operationPrecedence(factor,index)
  private def postfixGroup =  postfixInc | postfixDec | indexGroup
  private def unaryGroup = logicalNot | bitwiseNot | prefixInc | prefixDec | unaryNegate | unaryPlus | postfixGroup
  private def multiplyGroup = operationPrecedence(unaryGroup,times | divide | remainder)
  private def addGroup = operationPrecedence(multiplyGroup, plus | minus)
  private def bitwiseShiftGroup = operationPrecedence(addGroup,bitwiseLeftShift | bitwiseRightShift)
  private def nonStrictConditionGroup = operationPrecedence(bitwiseShiftGroup,lessThen | lessThenOrEq | greaterThen | greaterThenOrEq)
  private def strictConditionGroup = operationPrecedence(nonStrictConditionGroup,isEq | isNotEq )
  private def bitwiseAndGroup = operationPrecedence(strictConditionGroup,bitwiseAnd)
  private def bitwiseXorGroup = operationPrecedence(bitwiseAndGroup,bitwiseXor)
  private def bitwiseOrGroup = operationPrecedence(bitwiseXorGroup,bitwiseOr)
  private def logicalAndGroup = operationPrecedence(bitwiseOrGroup,logicalAnd)
  private def logicalOrGroup = operationPrecedence(logicalAndGroup,logicalOr)
  private def operationPrecedence (before: Parser[E], func : Parser[E=>E]) = before ~ rep(func) ^^ foldExpression
}

trait AssignmentParser extends RegexParsers { self: ExpressionParser with IdentifierParser =>
  implicit def variableToExpression(v: Variable):Expression = GerVar(v)

  def assignments = declareVars | assign | assignPlus |assignMinus |assignTimes |assignDivide |assignRemainder |
    assignLogicalNot|assignBitwiseNot | assignBitwiseRightShift |
    assignBitwiseAnd |assignBitwiseXor | assignBitwiseOr | assignLogicalAnd | assignLogicalOr

  def declareVars : Parser[DeclareVars] = "var" ~> repsep(newVar,",".r) ^^ (DeclareVars(_))
  def assign: Parser[`=`] = variable ~ "=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, a) }
  def assignPlus: Parser[`=`] = variable ~ "+=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `:+`(v, a)) }
  def assignMinus: Parser[`=`] = variable ~ "-=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `:-`(v, a)) }
  def assignTimes: Parser[`=`] = variable ~ "*=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `*`(v, a)) }
  def assignDivide: Parser[`=`] = variable ~ "/=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `/`(v, a)) }
  def assignRemainder: Parser[`=`] = variable ~ "%=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `%`(v, a)) }
  def assignLogicalNot: Parser[`=`] = variable ~ "!=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `!:`(a)) }
  def assignBitwiseNot: Parser[`=`] = variable ~ "~=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `~:`(a)) }
  def assignBitwiseLeftShift: Parser[`=`] = variable ~ "<<=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `>>`(v, a)) }
  def assignBitwiseRightShift: Parser[`=`] = variable ~ ">>=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `<<`(v, a)) }
  def assignBitwiseAnd: Parser[`=`] = variable ~ "&=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `&`(v, a)) }
  def assignBitwiseXor: Parser[`=`] = variable ~ "^=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `^`(v, a)) }
  def assignBitwiseOr: Parser[`=`] = variable ~ "|=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `|`(v, a)) }
  def assignLogicalAnd: Parser[`=`] = variable ~ "&&=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `&&`(v, a)) }
  def assignLogicalOr: Parser[`=`] = variable ~ "||=" ~ expression ^^ { case v ~ _ ~ a => `=`(v, `||`(v, a)) }
  private def newVar: Parser[`=`] = assign | nullVar
  private def nullVar: Parser[`=`] = variable ^^ { case v => `=`(v, LiteralExpression(NullLiteral)) }
}

trait LiteralParser extends RegexParsers { self: KeywordParser =>
  override def skipWhitespace: Boolean = true
  def nullLiteral =  nullKeyword ^^ (_ => NullLiteral)
  def booleanLiteral = (trueKeyword| falseKeyword) ^^ toBooleanLiteral
  def arrayLiteral = "[" ~> arrayItems
  def numericLiteral = notKeyword ~> (hexIntegerLiteral | decimalLiteral)
  def stringLiteral = notKeyword ~> (doubleQuoteStringLiteral | singleQuoteStringLiteral)
  def scriptLiteral = nullLiteral | booleanLiteral | numericLiteral | stringLiteral | arrayLiteral
  def scriptLiterals = rep(scriptLiteral)

  private def arrayItems :Parser[ArrayLiteral]= repsep(scriptLiteral,",") ~ "]" ^^ {case  x ~ _ => ArrayLiteral(Vector.empty[Literal] ++ x)}
  private def decimalLiteral= """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ toDecimalLiteral
  private def hexIntegerLiteral = "0(x|X)[0-9a-fA-F]+".r ^^ toHexDecimalLiteral
  private def doubleQuoteStringLiteral = "\"" ~> "[^\"\n\t]*".r <~ "\"" ^^ toStringLiteral
  private def singleQuoteStringLiteral = "'" ~> "[^'\n\t]*".r <~ "'" ^^ toStringLiteral
  private def toDecimalLiteral(x: String) = DecimalLiteral(BigDecimal(x))
  private def toHexDecimalLiteral(x: String) = DecimalLiteral(BigDecimal(Integer.parseInt(x.substring(2), 16)))
  private def toStringLiteral(x: String) = StringLiteral(StringEscapeUtils.unescapeJava(x))
  private def toBooleanLiteral(x: String) = BooleanLiteral(x.toBoolean)
}

trait IdentifierParser extends RegexParsers{self: KeywordParser =>
  type V = Variable
  val idName = "[a-zA-Z_]+([a-zA-Z0-9_])*".r
  def variable: Parser[V] = notKeyword ~>(globalVariable | localVariable)
  def globalVariable: Parser[V] = "$" ~> idName ^^ (GlobalVairable(_))
  def localVariable: Parser[V] = idName ^^ (LocalVariable(_))
}

trait KeywordParser extends RegexParsers{
  val ifKeyword = "if"
  val elseKeyword  = "else"
  val switchKeyword = "switch"
  val forKeyword = "for"
  val whileKeyword = "while"
  val caseKeyword = "case"
  val breakKeyword = "break"
  val continueKeyword = "continue"
  val defaultKeyword = "default"
  val varKeyword = "var"
  val doKeyword = "do"
  val nullKeyword = "null"
  val trueKeyword = "true"
  val falseKeyword = "false"

  def keyword = ifKeyword | elseKeyword | switchKeyword | caseKeyword | whileKeyword | forKeyword | breakKeyword | continueKeyword | defaultKeyword | varKeyword | doKeyword
  def notKeyword = not(keyword)
}

