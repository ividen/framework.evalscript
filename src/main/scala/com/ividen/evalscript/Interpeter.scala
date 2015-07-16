package com.ividen.evalscript

import scala.annotation.tailrec
import scala.collection.mutable

object Interpreter {

  class ScriptExecution(script: Script,globalContext: GlobalContext) {
    def process = new BlockExecution(script.block,globalContext).process
  }
  
  class BlockExecution(block: `{}`, globalContext: GlobalContext, rootLocalContext: Option[LocalContext] = None){
    val localContext = LocalContext(rootLocalContext)

    def process = block.items.foreach(processElement)

    private def processElement(e: ScriptElement):Unit = e match {
      case exp: Expression => processExpression(exp)
      case DeclareVars(l) =>l.foreach(declareVar)
      case assignment: `=` => processAssignment(assignment)
      case `if else`(i,e) => processIfElse(i,e)
      case b: `{}` => processNewBlock(b)
    }

    private def declareVar(e: `=`) = localContext.newVar(e.l, processExpression(e.r))
    private def processNewBlock(b: `{}`) = new BlockExecution(b, globalContext, Some(localContext)).process
    private def processIfElse(_if: `if`, _else: Seq[`else`]) = if (checkIf(_if)) processNewBlock(_if.block) else _else.find(checkElse).foreach(x => processNewBlock(x.block))
    private def checkIf(_if: `if`): Boolean = processCondition(_if.c)
    private def checkElse(_else: `else`): Boolean = _else.c.fold(true)(c => processCondition(c))
    private def processCondition(c: Expression): Boolean = processExpression(c) match {
      case DecimalLiteral(x) if x != 0 => true
      case StringLiteral(x) if !x.isEmpty => true
      case BooleanLiteral(x) if x => true
      case _ => false
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
      case `++:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v,LiteralExpression(result)));result
      case `--:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v,LiteralExpression(result)));result
      case `:++`(v) => val result = processExpression(GerVar(v)) ; processAssignment(`=`(v,LiteralExpression(result +  DecimalLiteral(BigDecimal(1)))));result
      case `:--`(v) => val result = processExpression(GerVar(v)) ; processAssignment(`=`(v,LiteralExpression(result -  DecimalLiteral(BigDecimal(1)))));result
      case `>>`(l, r) => processExpression(l) >> processExpression(r)
      case `<<`(l, r) => processExpression(l) << processExpression(r)
      case `&`(l, r) => processExpression(l) & processExpression(r)
      case `^`(l, r) => processExpression(l) ^ processExpression(r)
      case `|`(l, r) => processExpression(l) | processExpression(r)
      case `&&`(l, r) => processExpression(l) && processExpression(r)
      case `||`(l, r) => processExpression(l) || processExpression(r)
      case `:==`(l, r) => processExpression(l) == processExpression(r)
      case `:!=`(l, r) => processExpression(l) != processExpression(r)
      case `<`(l, r) => processExpression(l) < processExpression(r)
      case `>`(l, r) => processExpression(l) > processExpression(r)
      case `>=`(l, r) => processExpression(l) >= processExpression(r)
      case `<=`(l, r) => processExpression(l) <= processExpression(r)
      case GerVar(v: LocalVariable) => localContext(v)
      case  GerVar(v: GlobalVairable) => globalContext(v)
    }

    private def processAssignment(assignment: `=`) = (assignment.l,assignment.r) match {
      case (v:LocalVariable,e) => localContext.set(v,processExpression(e))
      case (g:GlobalVairable,e) => globalContext.set(g,processExpression(e))
    }
    
  }

  def process(script: Script, globalContext: GlobalContext) = new ScriptExecution(script,globalContext).process
}

class GlobalContext(initVars: Map[String,_] = Map.empty){
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
  def newVar(v: Variable,value: Literal) =vars.put(v.name , value)
  protected def findContext(v:LocalVariable): Option[LocalContext] = if(vars.contains(v.name)) Some(this) else parent.fold[Option[LocalContext]](None)(c => c.findContext(v))
}

object Main2 extends EvalScriptParser {
  def main(args: Array[String]) {
    val s =
      """
        |var k = 1, l = 100
        |$test_1_1 = k
        |{
        |  var k = 2*l
        |  $test_2_1 = k
        |  {
        |    var k=3*l
        |    $test_3_1 = k++
        |    {
        |       $test_4 = k+100
        |    }
        |  }
        |  $test_2_2 = k
        |}
        |
        |$test_1_2 = k
        |
        |if( (true && false) || (true || false))
        |  $result = true
        |else(true || false)
        |  $result = false
        |else
        |  $somes_else = true
        |
        |$resultString  = "\n"
        |if($test_1_1 < 100){
        |   $resultString += "$test_1_1 is equal " + $test_1_1 + "\n"
        |}
        |
        |if($test_1_2 < 100){
        |   $resultString += "$test_1_2 is equal " + $test_1_2 + "\n"
        |}
        |
        |if($test_2_1 < 100){
        |   $resultString += "$test_2_1 is equal " + $test_2_1 + "\n"
        |}
        |
        |if($test_2_2 < 100){
        |   $resultString += "$test_2_2 is equal " + $test_2_2 + "\n"
        |}
        |
        |if((10+10*20 - 90) > 8) $resultString += " && > then 8"
        |
        |
      """.stripMargin

    println(s)

    val i = 1


    val res = parseAll(script, s).get
    println(res)
    val context= new GlobalContext()
    Interpreter.process(res,context)
    println(context.vars)



  }
}
