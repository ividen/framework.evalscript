package com.viaden.crm.core.calculation.testfor

import java.io.{ByteArrayOutputStream, OutputStreamWriter, StringReader}

import freemarker.template.{Configuration, Template}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}


@State(Scope.Benchmark)
class FreeMarkerFor {
  def cycle(c: Int, p: Int): String =   (1 to c).fold("")((s,x) => s + s"\n <#list 1..${p} as i_${x}>") + " <#global result = result+1 >" +  (1 to c).fold("")((s,x) => s + s"\n </#list>")

  val s1_1000 = cycle(1, 1000)
  val s5_10 = cycle(5, 10)
  val s10_5 = cycle(10, 5)

  val params: Map[String, _] = Map("result" -> 1)

  val t1 = template(s1_1000)
  val t5 = template(s5_10)
  val t10 = template(s10_5)

  def apply(t: Template, params: Map[String, _]): String = {
    val m = new java.util.HashMap[String, Any](params.size)
    params.foreach((x) => m.put(x._1, x._2))
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
    val out = new OutputStreamWriter(baos)
    val env = t.createProcessingEnvironment(m, out)
    env.process()
    out.flush()
    baos.toString
  }

  def template(settings: String): Template = {
    val configuration = new Configuration(Configuration.VERSION_2_3_22)
    configuration.setTemplateUpdateDelay(0)
    configuration.setCacheStorage(new freemarker.cache.NullCacheStorage())
    new Template(settings, new StringReader(settings), configuration)

  }

  @Benchmark
  def for_1_1000(): Unit =  apply(t1, params)
  @Benchmark
  def for_5_10(): Unit =     apply(t5, params)
  @Benchmark
  def for_10_5(): Unit =     apply(t10, params)

}
