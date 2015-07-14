package com.ividen.evalscript

import scala.collection.mutable

object Interpreter {
  def process(elements: List[ProgramElement]) = elements.foreach(processElement)
  private def processElement(e: ProgramElement) = e match {
    case exp: Expression => println(processExpression(exp))
  }
  private def processExpression(e: Expression): Val = e match {
    case LiteralExpression(l) => Val(l.value)
    case PlusExpression(l, r) => processExpression(l) + processExpression(r)
    case MinusExpression(l, r) => processExpression(l) - processExpression(r)
    case DivideExpression(l, r) => processExpression(l) / processExpression(r)
    case MultiplyExpression(l, r) => processExpression(l) * processExpression(r)
    case ModExpression(l, r) => processExpression(l) % processExpression(r)
  }
}

case class Val(value: Any) {

  def +(v: Val): Val = (value, v.value) match {
    case (l: BigDecimal, r: BigDecimal) => Val(l + r)
    case (l: BigDecimal, r: String) => Val(l + BigDecimal(r))
    case (l: BigDecimal, r: Boolean) => Val(l + BigDecimal(if (r) 1 else 0))
    case (l: String, r: String) => Val(l + r)
    case (l: String, r: BigDecimal) => Val(l + r.toString)
    case (l: String, r: Boolean) => Val(l + r.toString)
    case x  => notSupported("+",x)
  }

  def -(v: Val): Val = (value, v.value) match {
    case (l: BigDecimal, r: BigDecimal) => Val(l - r)
    case (l: BigDecimal, r: String) => Val(l - BigDecimal(r))
    case (l: BigDecimal, r: Boolean) => Val(l - BigDecimal(if (r) 1 else 0))
    case x  => notSupported("-",x)
  }

  def *(v: Val): Val = (value, v.value) match {
    case (l: BigDecimal, r: BigDecimal) => Val(l * r)
    case (l: BigDecimal, r: String) => Val(l * BigDecimal(r))
    case (l: BigDecimal, r: Boolean) => Val(l * BigDecimal(if (r) 1 else 0))
    case x  => notSupported("*",x)
  }

  def /(v: Val): Val = (value, v.value) match {
    case (l: BigDecimal, r: BigDecimal) => Val(l * r)
    case (l: BigDecimal, r: String) => Val(l / BigDecimal(r))
    case (l: BigDecimal, r: Boolean) => Val(l / BigDecimal(if (r) 1 else 0))
    case x  => notSupported("/",x)
  }

  def %(v: Val): Val = (value, v.value) match {
    case (l: BigDecimal, r: BigDecimal) => Val(l * r)
    case (l: BigDecimal, r: String) => Val(l % BigDecimal(r))
    case (l: BigDecimal, r: Boolean) => Val(l % BigDecimal(if (r) 1 else 0))
    case x  => notSupported("%",x)
  }

  private def notSupported(o: String, x: (Any, Any)): Val  = throw new UnsupportedOperationException(s"Can't do  ${x._1}:${x._1.getClass.getName} ${o} ${x._2}:${x._2.getClass.getName}")
}

class GlobalContext(val vars: mutable.HashMap[String,String] = new mutable.HashMap[String,String]()){
  def apply(v: GlobalVairable): String  = vars.getOrElse(v.name, "")
  def set(v: GlobalVairable, value : String) = vars.put(v.name, value)
}

class ExecutionContext(val vars: mutable.HashMap[String,String] = new mutable.HashMap[String,String](), parent: Option[ExecutionContext] = None){
  def apply(v: LocalVariable): String  = vars.getOrElse(v.name, parent.fold("")(c => c.apply(v)))
  def set(v: LocalVariable, value : String) = findContext(v).fold(this)(_).vars.put(v.name , value)
  protected def findContext(v:LocalVariable): Option[ExecutionContext] = if(vars.contains(v.name)) Some(this) else parent.fold(None[ExecutionContext])(c => c.findContext(v))
}

object Main2 extends EvalScriptParser {
  def main(args: Array[String]) {
    val s = """ 'Test "1"' + " and " +"Test '2'" + (10 *10)  """

    val res = parseAll(program, s).get
    println(res)
    Interpreter.process(res)
  }
}
