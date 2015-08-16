package com.viaden.crm.core.calculation.testfor

import java.io.StringReader

import freemarker.template.{Configuration, Template}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import tcl.lang.{Interp, TCL}


@State(Scope.Benchmark)
class TclFor {
  def cycle(c: Int, p: Int): String = (1 to c).fold("")((s, x) => s + s"\n for {set i_${x} 0} {$$i_${x} < ${p}} {incr i_${x}} {") + "\n set result [expr $result + 1] " + (1 to c).fold("")((s, x) => s + s"\n }")

  val params: Map[String, Int] = Map("result" -> 1)
  val s1_1000 = cycle(1, 1000)
  val s5_10 = cycle(5, 10)
  val s10_5 = cycle(10, 5)

  val t1 = template(s1_1000)
  val t15 = template(s5_10)
  val t10 = template(s10_5)


  def apply(s: String, params: Map[String, Int]) = {
    val interp: Interp = new Interp

    params.foreach((x) => {
      interp.setVar(x._1, null, x._2, TCL.GLOBAL_ONLY)
    })
    interp.eval(s)
    interp.dispose()
  }

  def template(settings: String): Template = new Template("name", new StringReader(settings), new Configuration(Configuration.VERSION_2_3_22))
  @Benchmark
  def for_1_1000(): Unit = apply(s1_1000, params)
  @Benchmark
  def for_5_10(): Unit = apply(s5_10, params)
  @Benchmark
  def for_10_5(): Unit = apply(s10_5, params)
}
