package com.ividen.evalscript

import scala.collection.mutable

object Interpreter {
  def process(elements: List[ProgramElement]) = elements.foreach(processElement)
  private def processElement(e: ProgramElement) = e match {
    case exp: Expression => println(processExpression(exp))
  }
  private def processExpression(e: Expression): Literal = e match {
    case LiteralExpression(l: Literal) => l
    case `:+`(l, r) => processExpression(l) + processExpression(r)
    case `:-`(l, r) => processExpression(l) - processExpression(r)
    case `/`(l, r) => processExpression(l) / processExpression(r)
    case `*`(l, r) =>processExpression(l) * processExpression(r)
    case `%`(l, r) => processExpression(l) % processExpression(r)
    case `!:`(r) => !processExpression(r)
    case `~:`(r) => ~processExpression(r)
    case `-:`(r) => -processExpression(r)
    case `+:`(r) => +processExpression(r)
    //case PrefixIncrementExpression(r) => processExpression(l) + processExpression(r)
    //case PrefixDecrementExpression(r) => processExpression(l) + processExpression(r)
    case `>>`(l, r) => processExpression(l) >> processExpression(r)
    case `<<`(l, r) => processExpression(l) << processExpression(r)
    //case PostfixIncrementExpression(l) => processExpression(l) + processExpression(r)
    //case PostfixDecrementExpression(l) => processExpression(l) + processExpression(r)
    case `&`(l, r) => processExpression(l) & processExpression(r)
    case `^`(l, r) => processExpression(l) ^ processExpression(r)
    case `|`(l, r) => processExpression(l) | processExpression(r)
    case `&&`(l, r) => processExpression(l) && processExpression(r)
    case `||`(l, r) => processExpression(l) || processExpression(r)
  }
}

class GlobalContext(val vars: mutable.HashMap[String,String] = new mutable.HashMap[String,String]()){
  def apply(v: GlobalVairable): String  = vars.getOrElse(v.name, "")
  def set(v: GlobalVairable, value : String) = vars.put(v.name, value)
}

class ExecutionContext(val vars: mutable.HashMap[String,String] = new mutable.HashMap[String,String](), parent: Option[ExecutionContext] = None){
  def apply(v: LocalVariable): String  = vars.getOrElse(v.name, parent.fold("")(c => c.apply(v)))
  def set(v: LocalVariable, value : String) = findContext(v).fold(this)(x =>x).vars.put(v.name , value)
  protected def findContext(v:LocalVariable): Option[ExecutionContext] = if(vars.contains(v.name)) Some(this) else parent.fold[Option[ExecutionContext]](None)(c => c.findContext(v))
}

object Main2 extends EvalScriptParser {
  def main(args: Array[String]) {
    val s = """ 'Test "1"' + " and " +"Test '2'" + (10 *10)  """

    val script =
      """
        |if($purchases<100) multiplier = 2
        |else($purchase<200) multiplier = 3
        |else($purchase<300) multiplier = 4
        |else multiplier = 100
        |
        |$amount *= multiplier
        |
      """.stripMargin

    val res = parseAll(program, s).get
    println(res)
    Interpreter.process(res)
  }
}
