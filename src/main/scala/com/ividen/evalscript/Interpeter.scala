package com.ividen.evalscript

object Interpreter {
  def process(elements: List[ProgramElement]) = elements.foreach(processElement)
  private def processElement(e: ProgramElement) = e match {
    case exp: Expression => println(processExpression(exp))
  }
  private def processExpression(e: Expression): BigDecimal = e match {
    case LiteralExpression(l) => l.value.asInstanceOf[BigDecimal]
    case PlusExpression(l, r) => processExpression(l) + processExpression(r)
    case MinusExpression(l, r) => processExpression(l) - processExpression(r)
    case DivideExpression(l, r) => processExpression(l) / processExpression(r)
    case MultiplyExpression(l, r) => processExpression(l) * processExpression(r)
    case ModExpression(l, r) => processExpression(l) % processExpression(r)
  }
}


object Main2 extends EvalScriptParser{
  def main(args: Array[String]) {
     val s = "1+1+1+1-2-2-2"

     Interpreter.process(parseAll(program,s).get)
  }
}
