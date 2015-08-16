package com.viaden.crm.core.calculation.testfor

import java.io.{ByteArrayOutputStream, OutputStreamWriter, StringReader}

import org.apache.velocity.runtime.RuntimeSingleton
import org.apache.velocity.{Template, VelocityContext}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.collection.JavaConversions._
import scala.collection.mutable


@State(Scope.Benchmark)
class VelocityFor {
  def cycle(c: Int, p: Int): String = (1 to c).fold("")((s, x) => s + s"\n #foreach($$i_${x} in [1..${p}])") + "\n   #set($result = $result+1)" + (1 to c).fold("")((s, x) => s + s"\n #end")

  val s1_1000 = cycle(1, 1000)
  val s5_10 = cycle(5, 10)
  val s10_5 = cycle(10, 5)

  val t1 = template(s1_1000)
  val t5 = template(s5_10)
  val t10 = template(s10_5)


  def apply(t: Template, p: mutable.Map[String, _]) = {
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
    val writer: OutputStreamWriter = new OutputStreamWriter(baos)
    val context = new VelocityContext(p)
    t.merge(context, writer)
    writer.flush()
    baos.toString
  }

  def template(settings: String): Template = {
    val runtimeServices = RuntimeSingleton.getRuntimeServices()
    val reader = new StringReader(settings)
    val node = runtimeServices.parse(reader, "Template name")
    val template = new Template()
    template.setRuntimeServices(runtimeServices)
    template.setData(node)
    template.initDocument()
    template
  }

  @Benchmark
  def for_1_1000(): Unit = apply(t1, mutable.Map("result" -> 1))
  @Benchmark
  def for_5_10(): Unit = apply(t5, mutable.Map("result" -> 1))
  @Benchmark
  def for_10_5(): Unit = apply(t10, mutable.Map("result" -> 1))
}
