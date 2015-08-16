package com.viaden.crm.core.calculation.ifelse

import com.ividen.evalscript._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}


@State(Scope.Benchmark)
class EvalscriptElseIfCalculations {
  val purchasecount: String = "purchaseCount"
  val multiplier: String = "multiplier"
  val price: String = "price"

  def params(purchaseCount: Int, price: Int): Map[String, Literal] = Map(this.purchasecount -> DecimalLiteral(purchaseCount), this.price -> DecimalLiteral(price))

  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) + "\n$price = $multiplier*$price"

  def elseif(n: Int, p: String, c: String) = {
    var s: String = s"if ($$$p<1)\n  $$$c = $$$p+1\n"
    s += (2 to n - 1).map(x => s"else($$$p<$x)\n $$$c = $$$p+$x").mkString("\n")
    s += s"\nelse\n $$$c = $$$p+$n"
    s
  }

  val s10 = settings(10, 10)
  val s100 = settings(100, 10)
  val s1000 = settings(1000, 10)

  val t10 = template(s10)
  val t100 = template(s100)
  val t1000 = template(s1000)

  val ct10 = ScriptCompiler.compile(t10).getConstructor(classOf[scala.collection.immutable.Map[_, _]])
  val ct100 = ScriptCompiler.compile(t100).getConstructor(classOf[scala.collection.immutable.Map[_, _]])
  val ct1000 = ScriptCompiler.compile(t1000).getConstructor(classOf[scala.collection.immutable.Map[_, _]])

  val params10 = params(10, 10)
  val params100 = params(100, 10)
  val params1000 = params(1000, 10)

  def template(s: String): Script = EvalScriptParser.load(s)

  @Benchmark
  def interp_if_else_10(): Unit =  Interpreter.execute(t10, new GlobalContext(params10))
  @Benchmark
  def interp_if_else_100(): Unit = Interpreter.execute(t100, new GlobalContext(params100))
  @Benchmark
  def interp_if_else_1000(): Unit = Interpreter.execute(t1000, new GlobalContext(params1000))
  @Benchmark
  def compiled_if_else_10(): Unit =  ct10.newInstance(params10).execute
  @Benchmark
  def compiled_if_else_100(): Unit = ct100.newInstance(params100).execute
  @Benchmark
  def compiled_if_else_1000(): Unit = ct1000.newInstance(params1000).execute
}
