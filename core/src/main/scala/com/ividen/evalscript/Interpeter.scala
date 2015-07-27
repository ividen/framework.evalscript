package com.ividen.evalscript

import scala.annotation.tailrec
import scala.collection.mutable

object Interpreter {

  private[Interpreter] class Breakable{
    var markBreak = false
    var markContinue = false
    
    override def toString = s"Breakable($markBreak,$markContinue)"
  }

  class ScriptExecution(script: Script, globalContext: GlobalContext) {
    def process = new BlockExecution(script.block, globalContext).process
  }

  class BlockExecution(execBlock: `{}`, globalContext: GlobalContext, rootLocalContext: Option[LocalContext] = None, breakable: Option[Breakable] = None) {
    val localContext = LocalContext(rootLocalContext)

    def process = for (e <- execBlock.items if !broken) processElement(e)
    private def break = breakable.foreach(x => x.markBreak = true)
    private def continue = breakable.foreach(x => {
      x.markBreak = false; x.markContinue = false
    })
    private def breakContinue = breakable.foreach(x => {
      x.markBreak = true; x.markContinue = true
    })
    private def broken = breakable.fold(false)(_.markBreak)
    private def continued = breakable.fold(false)(_.markContinue)


    private def processElement(e: ScriptElement): Unit = e match {
      case exp: Expression => processExpression(exp)
      case DeclareVars(l) => l.foreach(declareVar)
      case assignment: `=` => processAssignment(assignment)
      case `if else`(i, e) => processIfElse(i, e)
      case `while do`(e, b,p) => processWhileDo(e, b,p)
      case `do while`(e, b) => processDoWhile(e, b)
      case `switch`(e, c, d) => processSwitch(e, c, d)
      case _: `break` => this.break
      case _: `continue` => this.breakContinue
      case b: `{}` => processNewBlock(b)
    }

    private def processSwitch(condition: Expression, cases: Seq[`case`], default: Option[`{}`]) ={
      val literal = processExpression(condition)
      @tailrec
      def findCase(i: Iterator[`case`]): Boolean = {
        if(i.hasNext){
          val next = i.next()
          if ((literal == processExpression(next.e)).toBooleanLiteral.value) {
            val breakable = new Breakable
            next.b.foreach(x =>new BlockExecution(x, globalContext, Some(localContext), Some(breakable)).process)
            while (!breakable.markBreak && i.hasNext) i.next.b.foreach(x =>new BlockExecution(x, globalContext, Some(localContext), Some(breakable)).process)
            true
          }else findCase(i)
        }else{
          false
        }
      }

      if(!findCase(cases.iterator))default.foreach(x =>processNewBlock(x))
    }

    private def declareVar(e: `=`) = localContext.newVar(e.l, processExpression(e.r))
    private def processNewBlock(b: `{}`) = new BlockExecution(b, globalContext, Some(localContext), breakable).process
    private def processIfElse(_if: `if`, _else: Seq[`else`]) = if (checkIf(_if)) processNewBlock(_if.block) else _else.find(checkElse).foreach(x => processNewBlock(x.block))
    private def checkIf(_if: `if`): Boolean = processCondition(_if.c)
    private def checkElse(_else: `else`): Boolean = _else.c.fold(true)(c => processCondition(c))
    private def processWhileDo(check: Expression, block: `{}`, postFix: Option[ScriptElement]) = workWhileDo(check,new BlockExecution(block, globalContext, Some(localContext),Some(new Breakable)),postFix)
    private def processDoWhile(check: Expression, block: `{}`) = workWhileDo(check,new BlockExecution(block, globalContext, Some(localContext),Some(new Breakable)),None,true)

    @tailrec
    private def workWhileDo(check: Expression,b: BlockExecution, postFix: Option[ScriptElement],firstCheckSkip : Boolean = false): Unit =
      if(firstCheckSkip || processCondition(check)){
        if(b.broken) {
          if (b.continued) {
            b.continue
            workWhileDo(check,b,postFix)
          }
        }else{
          b.process
          postFix.foreach(processElement)
          workWhileDo(check,b,postFix)
        }
      }

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
      case `[]`(l,r) => processExpression(l).apply(processExpression(r))
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
      case `call`(n,a) => FunctionInvoker.invoke(n,a.map(processExpression))
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
  def apply(v: GlobalVairable): Literal = apply(v.name)
  def apply(n: String) = vars.getOrElse(n, NullLiteral)
  def set(v: GlobalVairable, value: Literal):Unit = set(v.name, value)
  def set(v: String, value: Literal):Unit = vars.put(v, value)
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
    case l: Literal => l
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
