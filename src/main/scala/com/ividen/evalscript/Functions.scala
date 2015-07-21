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
