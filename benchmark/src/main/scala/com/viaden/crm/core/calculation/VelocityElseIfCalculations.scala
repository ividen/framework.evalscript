package com.viaden.crm.core.calculation

import java.io.{ByteArrayOutputStream, OutputStreamWriter, StringReader}

import org.apache.velocity.runtime.RuntimeSingleton
import org.apache.velocity.{Template, VelocityContext}
import org.openjdk.jmh.annotations.{Scope, State, Benchmark}

import scala.collection.JavaConversions._
import scala.collection.mutable


@State(Scope.Benchmark)
class VelocityElseIfCalculations {
  val purchasecount: String = "purchaseCount"
  val multiplier: String = "multiplier"
  val price: String = "price"

  def params(purchaseCount: Int, price: Int): mutable.Map[String, _] = {
    val result: mutable.Map[String,Any] = mutable.Map()
    result += this.purchasecount -> purchaseCount
    result += this.price -> price
    result
  }

  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) +  "\n#set($price = $multiplier*$price)"

  def elseif(n: Int, p: String, c: String) = {
    var s: String = s"#if ($$$p<1)\n  #set($$$c = $$$p+1)\n"
    s += (2 to n - 1).map(x => s"#elseif ($$$p<$x)\n  #set($$$c = $$$p+$x)").mkString("\n")
    s += s"\n#else\n  #set($$$c = $$$p+$n)\n#end"
    s
  }


  val s10 = settings(10, 10)
  val s100 = settings(100, 10)
  val s1000 =  settings(1000, 10)

  val t10 = template(s10)
  val t100 = template(s100)
  val t1000 = template(s1000)

  val params10 = params(10, 10)
  val params100 = params(100, 10)
  val params1000 = params(1000, 10)

  def apply(t: Template, p: mutable.Map[String, _]) = {
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
    val writer: OutputStreamWriter = new OutputStreamWriter(baos)
    t.merge(new VelocityContext(p), writer)
    writer.flush()
    baos.toString
  }

  def template(settings: String): Template = {
    val runtimeServices = RuntimeSingleton.getRuntimeServices();
    val reader = new StringReader(settings);
    val node = runtimeServices.parse(reader, "Template name");
    val template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(node);
    template.initDocument();
    template
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
}
