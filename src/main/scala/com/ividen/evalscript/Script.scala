package com.ividen.evalscript

sealed trait ScriptElement
case class Script(block: `{}`)

case class `{}`(items: Seq[ScriptElement]) extends ScriptElement
case class DeclareVars(items: Seq[`=`]) extends ScriptElement
case class `=`(l:Variable,r: Expression) extends ScriptElement

sealed trait Expression extends ScriptElement
case class LiteralExpression(literal: Literal) extends Expression
case class GerVar(variable:Variable) extends Expression
case class `call`(name: String, args: Seq[Expression]) extends Expression
case class `[]`(l: Expression,r: Expression) extends Expression
case class `:+`(l: Expression,r: Expression) extends Expression
case class `:-`(l: Expression,r: Expression) extends Expression
case class `/`(l: Expression,r: Expression) extends Expression
case class `*`(l: Expression,r: Expression) extends Expression
case class `%`(l: Expression,r: Expression) extends Expression
case class `!:`(r: Expression) extends Expression
case class `~:`(r: Expression) extends Expression
case class `-:`(r: Expression) extends Expression
case class `+:`(r: Expression) extends Expression
case class `++:`(r: Variable) extends Expression
case class `:++`(r: Variable) extends Expression
case class `--:`(r: Variable) extends Expression
case class `:--`(r: Variable) extends Expression
case class `<<`(l: Expression,r : Expression) extends Expression
case class `>>`(l: Expression,r : Expression) extends Expression
case class `++`(l: Expression) extends Expression
case class `--`(l: Expression) extends Expression
case class `&`(l: Expression, r: Expression) extends Expression
case class `^`(l: Expression, r: Expression) extends Expression
case class `|`(l: Expression, r: Expression) extends Expression
case class `&&`(l: Expression,r: Expression) extends Expression
case class `||`(l: Expression, r: Expression) extends Expression
case class `:==`(l: Expression, r: Expression) extends Expression
case class `:!=`(l: Expression, r: Expression) extends Expression
case class `<`(l: Expression, r: Expression) extends Expression
case class `>`(l: Expression, r: Expression) extends Expression
case class `>=`(l: Expression, r: Expression) extends Expression
case class `<=`(l: Expression, r: Expression) extends Expression

case class `if`(c : Expression, block: `{}`)
case class `else`(c: Option[Expression],block:`{}`)
case class `if else`(i: `if`, e: Seq[`else`]) extends ScriptElement

case class `while do`(e: Expression, block: `{}`, postFix: Option[ScriptElement] = None) extends ScriptElement
case class `do while`(e: Expression, block: `{}`) extends ScriptElement
case class `for`(init: ScriptElement, check: Expression, postfix: Expression, block: `{}`) extends ScriptElement
case class `case`(e: Expression, b: Option[`{}`])
case class `switch`(e: Expression, cases: Seq[`case`], default: Option[`{}`]) extends ScriptElement
class `break` extends ScriptElement
class `continue` extends ScriptElement

sealed trait ExpressionElement extends ScriptElement

sealed trait Variable extends ExpressionElement{
  def name : String
}
case class LocalVariable(name: String) extends Variable

case class GlobalVairable(name: String) extends Variable

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
  def ==(l: Literal) : Literal = unsupportedOperation
  def !=(l: Literal) : Literal = unsupportedOperation
  def <(l: Literal) : Literal = unsupportedOperation
  def >(l: Literal) : Literal = unsupportedOperation
  def >=(l: Literal) : Literal = unsupportedOperation
  def <=(l: Literal) : Literal = unsupportedOperation
  def apply(l: Literal): Literal = unsupportedOperation

  def toBooleanLiteral: BooleanLiteral = unsupportedOperation
  def toStringLiteral : StringLiteral= unsupportedOperation
  def toDecimalLiteral: DecimalLiteral= unsupportedOperation
  def toArrayLiteral: ArrayLiteral = unsupportedOperation

  private def unsupportedOperation[U]: U = throw new UnsupportedOperationException("Operation is not supported!")
}

object NullLiteral extends Literal {
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
  override def ==(l: Literal) : Literal = BooleanLiteral(value == l.toBooleanLiteral.value)
  override def !=(l: Literal) : Literal = BooleanLiteral(value != l.toBooleanLiteral.value)
  override def <(l: Literal) : Literal = BooleanLiteral(value < l.toBooleanLiteral.value)
  override def >(l: Literal) : Literal = BooleanLiteral(value > l.toBooleanLiteral.value)
  override def >=(l: Literal) : Literal = BooleanLiteral(value >= l.toBooleanLiteral.value)
  override def <=(l: Literal) : Literal = BooleanLiteral(value <= l.toBooleanLiteral.value)
  override def apply(l: Literal): Literal =  this.toArrayLiteral.apply(l)

  override def toBooleanLiteral: BooleanLiteral = this
  override def toStringLiteral: StringLiteral = StringLiteral(value.toString)
  override def toDecimalLiteral: DecimalLiteral = DecimalLiteral(BigDecimal(if(value) 1 else 0))
  override def toArrayLiteral: ArrayLiteral = ArrayLiteral(Vector(this))
}

case class DecimalLiteral(value: BigDecimal) extends Literal {
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
  override def ==(l: Literal) : Literal = BooleanLiteral(value == l.toDecimalLiteral.value)
  override def !=(l: Literal) : Literal = BooleanLiteral(value != l.toDecimalLiteral.value)
  override def <(l: Literal) : Literal = BooleanLiteral(value < l.toDecimalLiteral.value)
  override def >(l: Literal) : Literal = BooleanLiteral(value > l.toDecimalLiteral.value)
  override def >=(l: Literal) : Literal = BooleanLiteral(value >= l.toDecimalLiteral.value)
  override def <=(l: Literal) : Literal = BooleanLiteral(value <= l.toDecimalLiteral.value)
  override def apply(l: Literal): Literal =  this.toArrayLiteral.apply(l)
  override def toBooleanLiteral: BooleanLiteral = BooleanLiteral(if (value == 0) false else true)
  override def toStringLiteral: StringLiteral = StringLiteral(value.toString)
  override def toDecimalLiteral: DecimalLiteral = this
  override def toArrayLiteral: ArrayLiteral = ArrayLiteral(Vector(this))
}
case class StringLiteral(value: String) extends Literal{
  type T = String

  override def +(l: Literal): Literal = StringLiteral(this.value + l.toStringLiteral.value)
  override def *(l: Literal): Literal = StringLiteral((1 to l.toDecimalLiteral.value.toInt).foldLeft[String](this.value)((x1,x2) => x1 + value))
  override def ==(l: Literal) : Literal = BooleanLiteral(value == l.toStringLiteral.value)
  override def !=(l: Literal) : Literal = BooleanLiteral(value != l.toStringLiteral.value)
  override def <(l: Literal) : Literal = BooleanLiteral(value < l.toStringLiteral.value)
  override def >(l: Literal) : Literal = BooleanLiteral(value > l.toStringLiteral.value)
  override def >=(l: Literal) : Literal = BooleanLiteral(value >= l.toStringLiteral.value)
  override def <=(l: Literal) : Literal = BooleanLiteral(value <= l.toStringLiteral.value)
  override def apply(l: Literal): Literal =  StringLiteral(String.valueOf(value.apply(l.toDecimalLiteral.value.toInt)))
  override def toBooleanLiteral: BooleanLiteral = BooleanLiteral(if (value == 0) false else true)
  override def toStringLiteral: StringLiteral = this
  override def toDecimalLiteral: DecimalLiteral = DecimalLiteral(BigDecimal(value))
  override def toArrayLiteral: ArrayLiteral = ArrayLiteral(Vector(this))
}


case class ArrayLiteral(value: Vector[Literal]) extends Literal {
  type T = Vector[Literal]

  override def +(l: Literal): Literal = ArrayLiteral(this.value ++ l.toArrayLiteral.value)
  override def apply(l: Literal): Literal = value.apply(l.toDecimalLiteral.value.toInt)
  override def toArrayLiteral: ArrayLiteral = this
}
