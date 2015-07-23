package com.ividen.evalscript

trait Functions{
  def bool(literal: Literal): BooleanLiteral = literal.toBooleanLiteral
  def str(literal: Literal): StringLiteral = literal.toStringLiteral
  def decimal(literal: Literal): DecimalLiteral = literal.toDecimalLiteral

  def len(l: Literal): Literal  = l match {
    case StringLiteral(v) => DecimalLiteral(v.length)
    case v => DecimalLiteral(v.toArrayLiteral.value.length)
  }
  def substring(src: Literal, begin: Literal, end: Literal): StringLiteral = StringLiteral(src.toStringLiteral.value.substring(begin.toDecimalLiteral.value.toInt, end.toDecimalLiteral.value.toInt))
  def toUpperCase(s: Literal): StringLiteral = StringLiteral(s.toStringLiteral.value.toUpperCase)
  def toLowerCase(s: Literal): StringLiteral = StringLiteral(s.toStringLiteral.value.toLowerCase)
  def indexOf(src: Literal, s: Literal): DecimalLiteral = DecimalLiteral(src.toStringLiteral.value.indexOf(s.toStringLiteral.value))


  def E() : Literal =  DecimalLiteral(math.E)
  def Pi() : Literal=  DecimalLiteral(math.Pi)
  
  def random() : Literal= DecimalLiteral(math.random)
  def sin(x : Literal) : Literal= DecimalLiteral(math.sin(x.toDecimalLiteral.value.toDouble))
  def cos(x : Literal) : Literal= DecimalLiteral(math.cos(x.toDecimalLiteral.value.toDouble))
  def tan(x : Literal) : Literal= DecimalLiteral(math.tan(x.toDecimalLiteral.value.toDouble))
  def asin(x : Literal) : Literal= DecimalLiteral(math.asin(x.toDecimalLiteral.value.toDouble))
  def acos(x : Literal) : Literal= DecimalLiteral(math.acos(x.toDecimalLiteral.value.toDouble))
  def atan(x : Literal) : Literal= DecimalLiteral(math.atan(x.toDecimalLiteral.value.toDouble))
  def toRadians(x : Literal) : Literal= DecimalLiteral(math.toRadians(x.toDecimalLiteral.value.toDouble))
  def toDegrees(x : Literal) : Literal= DecimalLiteral(math.toDegrees(x.toDecimalLiteral.value.toDouble))
  def exp(x : Literal) : Literal= DecimalLiteral(math.exp(x.toDecimalLiteral.value.toDouble))
  def log(x : Literal) : Literal= DecimalLiteral(math.log(x.toDecimalLiteral.value.toDouble))
  def sqrt(x : Literal) : Literal= DecimalLiteral(math.sqrt(x.toDecimalLiteral.value.toDouble))
  def IEEEremainder(x : Literal, y : Literal) : DecimalLiteral= DecimalLiteral(math.IEEEremainder(x.toDecimalLiteral.value.toDouble,y.toDecimalLiteral.value.toDouble))
  def ceil(x : Literal) : Literal= DecimalLiteral(math.ceil(x.toDecimalLiteral.value.toDouble))
  def floor(x : Literal) : Literal= DecimalLiteral(math.floor(x.toDecimalLiteral.value.toDouble))
  def rint(x : Literal) : Literal= DecimalLiteral(math.floor(x.toDecimalLiteral.value.toDouble))
  def atan2(y : Literal, x : Literal) : DecimalLiteral= DecimalLiteral(math.IEEEremainder(x.toDecimalLiteral.value.toDouble,y.toDecimalLiteral.value.toDouble))
  def pow(x : Literal, y : Literal) : DecimalLiteral= DecimalLiteral(math.pow(x.toDecimalLiteral.value.toDouble, y.toDecimalLiteral.value.toDouble))
  def round(x : Literal) :Literal = DecimalLiteral(math.round(x.toDecimalLiteral.value.toDouble))
  def abs(x : Literal) : Literal = DecimalLiteral(x.toDecimalLiteral.value.abs)
  def max(x : Literal, y : Literal) : Literal = DecimalLiteral(x.toDecimalLiteral.value.max(y.toDecimalLiteral.value))
  def signum(x : Literal) : Literal = DecimalLiteral(x.toDecimalLiteral.value.signum)
  def cbrt(x : Literal) : DecimalLiteral= DecimalLiteral(math.cbrt(x.toDecimalLiteral.value.toDouble))
  def expm1(x : Literal) : DecimalLiteral= DecimalLiteral(math.expm1(x.toDecimalLiteral.value.toDouble))
  def log1p(x : Literal) : DecimalLiteral= DecimalLiteral(math.log1p(x.toDecimalLiteral.value.toDouble))
  def log10(x : Literal) : DecimalLiteral= DecimalLiteral(math.log10(x.toDecimalLiteral.value.toDouble))
  def sinh(x : Literal) : DecimalLiteral= DecimalLiteral(math.sinh(x.toDecimalLiteral.value.toDouble))
  def cosh(x : Literal) : DecimalLiteral= DecimalLiteral(math.cosh(x.toDecimalLiteral.value.toDouble))
  def tanh(x : Literal) : DecimalLiteral= DecimalLiteral(math.tanh(x.toDecimalLiteral.value.toDouble))
  def hypot(x : Literal, y : DecimalLiteral) : DecimalLiteral= DecimalLiteral(math.hypot(x.toDecimalLiteral.value.toDouble,y.toDecimalLiteral.value.toDouble))
  def ulp(x : Literal) : DecimalLiteral= DecimalLiteral(math.ulp(x.toDecimalLiteral.value.toDouble))
}

object Functions extends Functions{
}

object FunctionInvoker{
  import scala.reflect.runtime._
  val (instance,methods) = {
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val funcModule = runtimeMirror.reflectModule(runtimeMirror.moduleSymbol(Functions.getClass))
    val clazz = runtimeMirror.reflectClass(runtimeMirror.classSymbol(Functions.getClass))
    val tag = universe.typeOf[Functions]
    (runtimeMirror.reflect(funcModule.instance),tag.declarations.filter(x => x.isMethod && !x.asMethod.isConstructor).map(x => x.asMethod.name.decodedName.toString -> x.asMethod).toMap)
  }

  def hasMethod(name: String) = methods.contains(name)
  def invoke(name: String, args: Seq[Literal]) : Literal =  methods.get(name).fold[Literal](NullLiteral)(x => instance.reflectMethod(x).apply(args:_*).asInstanceOf[Literal])
}
