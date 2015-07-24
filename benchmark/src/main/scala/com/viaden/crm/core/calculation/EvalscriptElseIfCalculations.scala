package com.viaden.crm.core.calculation

import com.ividen.evalscript._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}


//@State(Scope.Benchmark)
//class EvalscriptElseIfCalculations {
//  val purchasecount: String = "purchaseCount"
//  val multiplier: String = "multiplier"
//  val price: String = "price"
//
//  def params(purchaseCount: Int, price: Int): Map[String, Int] = Map(this.purchasecount -> purchaseCount, this.price -> price)
//
//  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) + "\n$price = $multiplier*$price"
//
//  def elseif(n: Int, p: String, c: String) = {
//    var s: String = s"if ($$$p<1)\n  $$$c = $$$p+1\n"
//    s += (2 to n - 1).map(x => s"else($$$p<$x)\n $$$c = $$$p+$x").mkString("\n")
//    s += s"\nelse\n $$$c = $$$p+$n"
//    s
//  }
//
//
//  val s10 = settings(20, 10)
//  val s100 = settings(100, 10)
//  val s1000 = settings(1000, 10)
//
//  val t10 = template(s10)
//  val t100 = template(s100)
//  val t1000 = template(s1000)
//
//  val ct10 = ScriptCompiler.compile(t10)
//  val ct100 = ScriptCompiler.compile(t100)
//  val ct1000 = ScriptCompiler.compile(t1000)
//
//  val params10 = params(20, 10)
//  val params100 = params(100, 10)
//  val params1000 = params(1000, 10)
//
//  def template(s: String): Script = EvalScriptParser.load(s)
//
//    @Benchmark
//    def interp_if_else_10(): Unit = Interpreter.process(t10, new GlobalContext(params10))
//  //  @Benchmark
//  //  def interp_if_else_100(): Unit = Interpreter.process(t100, new GlobalContext(params100))
//  //  @Benchmark
//  //  def interp_if_else_1000(): Unit = Interpreter.process(t1000, new GlobalContext(params1000))
//    @Benchmark
//    def compiled_if_else_10(): Unit = ct10.getConstructor(classOf[GlobalContext]).newInstance(new GlobalContext(params10)).execute
//  //  @Benchmark
//  //  def compiled_if_else_100(): Unit = ct100.getConstructor(classOf[GlobalContext]).newInstance(new GlobalContext(params100)).execute
////  @Benchmark
////  def compiled_if_else_1000(): Unit = {
////    val instance = ct1000.getConstructor(classOf[GlobalContext]).newInstance(new GlobalContext(params1000))
////    instance.execute
////  }
//
//}
