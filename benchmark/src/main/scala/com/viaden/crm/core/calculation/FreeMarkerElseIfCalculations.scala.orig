package com.viaden.crm.core.calculation

import java.io.{StringReader, OutputStreamWriter, ByteArrayOutputStream}

import freemarker.template.{Configuration, Template}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.collection.mutable
import scala.collection.JavaConversions._


@State(Scope.Benchmark)
class FreeMarkerElseIfCalculations {
  val purchasecount: String = "purchaseCount"
  val multiplier: String = "multiplier"
  val price: String = "price"

  def params(purchaseCount: Int, price: Int): mutable.Map[String, _] = {
    val result: mutable.Map[String, Any] = mutable.Map()
    result += this.purchasecount -> purchaseCount
    result += this.price -> price
    result
  }

  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) +  "\n<#global price = multiplier*price >"

  def elseif(n: Int, p: String, c: String) = {
    var s: String = s"<#if $p lt 1 >\n  <#global $c = $p+1 >\n"
    s += (2 to n - 1).map(x => s"<#elseif $p lt $x >\n  <#global $c = $p+$x >").mkString("\n")
    s += s"\n<#elseif  $p lt ${n+1}>\n  <#global $c = $p+$n >\n</#if>"
    s
  }

  val s10 = settings(10, 10)
  val s100 =settings(100, 10)
  val s1000 =settings(1000, 10)

  val t10 = template(s10)
  val t100 = template(s100)
  val t1000 = template(s1000)

  val params10 = params(10, 10)
  val params100 = params(100, 10)
  val params1000 = params(1000, 10)


  def apply(t: Template, params: mutable.Map[String, _]): String = {
    val m = new java.util.HashMap[String, Any](params.size)
    params.foreach((x) => m.put(x._1, x._2))
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
    val out = new OutputStreamWriter(baos);
    val env = t.createProcessingEnvironment(m, out)
    env.process()
    out.flush()
    baos.toString
  }

  def template(settings: String): Template = {
    val configuration = new Configuration(Configuration.VERSION_2_3_22)
    configuration.setTemplateUpdateDelay(0)
//    configuration.setSetting(Configuration.CACHE_STORAGE_KEY, "strong:0, soft:0");
    configuration.setCacheStorage(new freemarker.cache.NullCacheStorage())
    new Template(settings, new StringReader(settings), configuration)

  }

  @Benchmark
  def if_else_10(): Unit = {
    apply(t10, params10)
  }

  @Benchmark
  def if_else_100(): Unit = {
    apply(t100, params100)
  }

  @Benchmark
  def if_else_1000(): Unit = {
    apply(t1000, params1000)
  }

  def main(args: Array[String]) {
      if_else_10()
  }
}
