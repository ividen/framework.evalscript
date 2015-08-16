package com.viaden.crm.core.calculation.testfor

import com.ividen.evalscript._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}


@State(Scope.Benchmark)
class EvalscriptFor {
  def cycle(c: Int, p: Int): String = (1 to c).foldLeft("")((s, x) => s + s"\nfor(var i${x}=0;i${x}<${p};i${x}++)") + "\n $result++"

  val params: Map[String, Int] = Map("result" -> 1)

  val s1_1000 = cycle(1, 1000)
  val s5_10 = cycle(5, 10)
  val s10_5 = cycle(10, 5)

  val t1 = template(s1_1000)
  val t5 = template(s5_10)
  val t10 = template(s10_5)

  val ct1 = ScriptCompiler.compile(t1).getConstructor(classOf[scala.collection.immutable.Map[_, _]])
  val ct5 = ScriptCompiler.compile(t5).getConstructor(classOf[scala.collection.immutable.Map[_, _]])
  val ct10 = ScriptCompiler.compile(t10).getConstructor(classOf[scala.collection.immutable.Map[_, _]])

  def template(s: String): Script = EvalScriptParser.load(s)

  @Benchmark
  def interp_for_1_1000(): Unit = Interpreter.execute(t1, new GlobalContext(params))
  @Benchmark
  def compiled_for_1_1000(): Unit = ct1.newInstance(params).execute
  @Benchmark
  def interp_for_5_10(): Unit = Interpreter.execute(t5, new GlobalContext(params))
  @Benchmark
  def compiled_for_5_10(): Unit = ct5.newInstance(params).execute
  @Benchmark
  def interp_for_10_5(): Unit = Interpreter.execute(t10, new GlobalContext(params))
  @Benchmark
  def compiled_for_10_5(): Unit = ct10.newInstance(params).execute
}
