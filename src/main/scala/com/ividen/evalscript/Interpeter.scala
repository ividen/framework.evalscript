package com.ividen.evalscript

import scala.collection.mutable

object Interpreter {

  class ScriptExecution(elements: List[ProgramElement],executionContext: ExecutionContext) {
    def process = elements.foreach(processElement)

    private def processElement(e: ProgramElement) = e match {
      case exp: Expression => println(processExpression(exp))
      case _ => throw new RuntimeException()
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
      case assignment: `=` => processAssignment(assignment); NullLiteral  //todo aguzanov ProgramElement?
    }

    private def processAssignment(assignment: `=`) = (assignment.l,assignment.r) match {
      case (v:LocalVariable,e) => executionContext.localRoot.set(v,processExpression(e))
      case (g:GlobalVairable,e) => executionContext.global.set(g,processExpression(e))
    }
  }

  def process(elements: List[ProgramElement], executionContext: ExecutionContext) = new ScriptExecution(elements,executionContext).process

}

class GlobalContext(initVars: Map[String,_]){
  var vars = mutable.Map[String,Literal]() ++ initVars.map(e => e._1 -> valToLiteral(e._2))
  def apply(v: GlobalVairable): Literal  = vars.getOrElse(v.name, NullLiteral)
  def set(v: GlobalVairable, value : Literal) = vars.put(v.name, value)
  private def valToLiteral(value: Any) = value match {
    case s: String => StringLiteral(s)
    case x: Int => DecimalLiteral(BigDecimal(x))
    case x: Short => DecimalLiteral(BigDecimal(x))
    case x: Long => DecimalLiteral(BigDecimal(x))
    case x: Byte => DecimalLiteral(BigDecimal(x))
    case x: Float => DecimalLiteral(BigDecimal(x))
    case x: Double => DecimalLiteral(BigDecimal(x))
    case null  => NullLiteral
    case x: Boolean => BooleanLiteral(x)
    case x => throw new IllegalArgumentException(s"Can't use $value for emaluation!")
  }
}

case class LocalContext(parent: Option[LocalContext] = None){
  val vars: mutable.HashMap[String,Literal] = new mutable.HashMap[String,Literal]()
  
  def apply(v: LocalVariable): Literal  = vars.getOrElse(v.name, parent.fold[Literal](NullLiteral)(c => c.apply(v)))
  def set(v: LocalVariable, value : Literal) = findContext(v).fold(this)(x =>x).vars.put(v.name , value)
  def newVar(v: LocalVariable,value: Literal) = vars.put(v.name , value)
  protected def findContext(v:LocalVariable): Option[LocalContext] = if(vars.contains(v.name)) Some(this) else parent.fold[Option[LocalContext]](None)(c => c.findContext(v))
}

case class ExecutionContext(globals: Map[String,_] = Map.empty){
  val global = new  GlobalContext(globals)
  val localRoot = LocalContext()
}

object Main2 extends EvalScriptParser {
  def main(args: Array[String]) {
    val s = """ 'Test "1"' + " and " +"Test '2'" + (10 *10)  """

    val script =
      """
        |if($purchases<100)  multiplier = 2
        |else($purchase<200) multiplier = 3
        |else($purchase<300) multiplier = 4
        |else multiplier = 100
        |
        |$amount *= multiplier
        |
      """.stripMargin

    val res = parseAll(program, s).get
    println(res)
    val context = ExecutionContext()
    Interpreter.process(res,context)
  }
}
