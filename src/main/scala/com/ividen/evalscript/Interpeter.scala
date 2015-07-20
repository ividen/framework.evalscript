package com.ividen.evalscript

import scala.annotation.tailrec
import scala.collection.mutable

object Interpreter {

  class ScriptExecution(script: Script, globalContext: GlobalContext) {
    def process = new BlockExecution(script.block, globalContext).process
  }

  class BlockExecution(block: `{}`, globalContext: GlobalContext, rootLocalContext: Option[LocalContext] = None) {
    val localContext = LocalContext(rootLocalContext)

    def process = block.items.foreach(processElement)

    private def processElement(e: ScriptElement): Unit = e match {
      case exp: Expression => processExpression(exp)
      case DeclareVars(l) => l.foreach(declareVar)
      case assignment: `=` => processAssignment(assignment)
      case `if else`(i, e) => processIfElse(i, e)
      case `while do`(e, b) => processWhileDo(e, b)  //todo barboza check break
      case `do while`(e, b) => processDoWhile(e, b)  //todo barboza check break
      case `switch`(e,c,d) => processSwitch(e,c,d)
      case b: `{}` => processNewBlock(b)
    }

    private def processSwitch(condition: Expression, cases: Seq[`case`], default: Option[`{}`]) ={
      val literal = processExpression(condition)
      @tailrec
      def findCase(i: Iterator[`case`]): Boolean = {
        if(i.hasNext){

          val next = i.next()
          if ((literal == processExpression(next.e)).toBooleanLiteral.value) {

            next.b.foreach(processNewBlock)
            while (i.hasNext) i.next.b.foreach(processNewBlock)
            true
          }else findCase(i)
        }else{
          false
        }
      }

      if(!findCase(cases.iterator))default.foreach(processNewBlock)
    }

    private def declareVar(e: `=`) = localContext.newVar(e.l, processExpression(e.r))
    private def processNewBlock(b: `{}`) = new BlockExecution(b, globalContext, Some(localContext)).process
    private def processIfElse(_if: `if`, _else: Seq[`else`]) = if (checkIf(_if)) processNewBlock(_if.block) else _else.find(checkElse).foreach(x => processNewBlock(x.block))
    private def checkIf(_if: `if`): Boolean = processCondition(_if.c)
    private def checkElse(_else: `else`): Boolean = _else.c.fold(true)(c => processCondition(c))
    private def processWhileDo(check: Expression, block: `{}`): Unit = while (processCondition(check)) processNewBlock(block)
    private def processDoWhile(check: Expression, block: `{}`): Unit = do processNewBlock(block) while (processCondition(check))
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
      case `*`(l, r) => processExpression(l) * processExpression(r)
      case `%`(l, r) => processExpression(l) % processExpression(r)
      case `!:`(r) => !processExpression(r)
      case `~:`(r) => ~processExpression(r)
      case `-:`(r) => -processExpression(r)
      case `+:`(r) => +processExpression(r)
      case `++:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      case `--:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      case `:++`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result + DecimalLiteral(BigDecimal(1))))); result
      case `:--`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result - DecimalLiteral(BigDecimal(1))))); result
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
      case GerVar(v: GlobalVairable) => globalContext(v)
    }

    private def processAssignment(assignment: `=`) = (assignment.l, assignment.r) match {
      case (v: LocalVariable, e) => localContext.set(v, processExpression(e))
      case (g: GlobalVairable, e) => globalContext.set(g, processExpression(e))
    }

  }

  def process(script: Script, globalContext: GlobalContext) = new ScriptExecution(script, globalContext).process
}

class GlobalContext(initVars: Map[String, _] = Map.empty) {
  var vars = mutable.Map[String, Literal]() ++ initVars.map(e => e._1 -> valToLiteral(e._2))
  def apply(v: GlobalVairable): Literal = vars.getOrElse(v.name, NullLiteral)
  def set(v: GlobalVairable, value: Literal) = vars.put(v.name, value)
  private def valToLiteral(value: Any) = value match {
    case s: String => StringLiteral(s)
    case x: Int => DecimalLiteral(BigDecimal(x))
    case x: Short => DecimalLiteral(BigDecimal(x))
    case x: Long => DecimalLiteral(BigDecimal(x))
    case x: Byte => DecimalLiteral(BigDecimal(x))
    case x: Float => DecimalLiteral(BigDecimal(x))
    case x: Double => DecimalLiteral(BigDecimal(x))
    case null => NullLiteral
    case x: Boolean => BooleanLiteral(x)
    case x => throw new IllegalArgumentException(s"Can't use $value for emaluation!")
  }
}

case class LocalContext(parent: Option[LocalContext] = None) {
  val vars: mutable.HashMap[String, Literal] = new mutable.HashMap[String, Literal]()

  def apply(v: LocalVariable): Literal = vars.getOrElse(v.name, parent.fold[Literal](NullLiteral)(c => c.apply(v)))
  def set(v: LocalVariable, value: Literal) = findContext(v).fold(this)(x => x).vars.put(v.name, value)
  def newVar(v: Variable, value: Literal) = vars.put(v.name, value)
  protected def findContext(v: LocalVariable): Option[LocalContext] = if (vars.contains(v.name)) Some(this) else parent.fold[Option[LocalContext]](None)(c => c.findContext(v))
}

object Main2 extends EvalScriptParser {
  def main(args: Array[String]) {

    val s =
      """
        |
        |
        |if($purchaseAmount<100) $multiplier = 1
        |else($purchaseAmount<200) $multiplier = 2
        |else($purchaseAmount<300) $multiplier = 3
        |else($purchaseAmount<400) $multiplier = 4
        |else($purchaseAmount<500) $multiplier = 5
        |else($purchaseAmount<600) $multiplier = 6
        |else($purchaseAmount<700) $multiplier = 7
        |else($purchaseAmount<800) $multiplier = 8
        |else($purchaseAmount<900) $multiplier = 9
        |else($purchaseAmount<1000) $multiplier = 10
        |else($purchaseAmount<1100) $multiplier = 11
        |else($purchaseAmount<1200) $multiplier = 12
        |else($purchaseAmount<1300) $multiplier = 13
        |else($purchaseAmount<1400) $multiplier = 14
        |else($purchaseAmount<1500) $multiplier = 15
        |else($purchaseAmount<1600) $multiplier = 16
        |else $multiplier = 17
        |
        |$amount  = 100 * $multiplier
        |
        |if(true) $its_true = "YES"
        |
        |$iterationCount = 0
        |while($amount>100){
        |  $amount -=1
        |  $iterationCount++
        |}
        |
        |var i =  $iterationCount
        |$iteractionCount2 = 0
        |while(i>0){
        |  $amount++
        |  $iteractionCount2 ++
        |  i--
        |}
        |
        |$amount2 = 0
        |for(j = 0; j<100 ;j++){
        |  for(i =0 ; i <100 ; i++){
        |     $amount  += i
        |     $amount2 += j
        |  }
        |}
        |
        |$result = ""
        |
        |switch($purchaseAmount) {
        |  case 10: $result += "& =10"
        |  case 20: {
        |     for(var i = 0; i<10;i++) $result +='_'
        |
        |     $result += " & 20"
        |  }
        |  case 100: $result += "& =100"
        |  case 200: $result += "& =200"
        |  case 300: $result += "& =300"
        |  default: $result += "& default"
        |}

        |
        |
      """.stripMargin

    println(s)

    val i = 1


    val res = parseAll(script, s).get
    println(res)
    val context = new GlobalContext(Map[String, Any]("purchaseAmount" -> 20))

    Interpreter.process(res, context)
    Console.readChar()
    for(j <- (1 to 100))
    Interpreter.process(res, context)

    println(context.vars)



  }
}
