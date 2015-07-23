package com.viaden.crm.core.calculation

import java.io.{ByteArrayOutputStream, OutputStreamWriter, StringReader}

import com.ividen.evalscript.{GlobalContext, Interpreter, EvalScriptParser, Script}
import org.apache.velocity.runtime.RuntimeSingleton
import org.apache.velocity.{Template, VelocityContext}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.collection.JavaConversions._
import scala.collection.mutable


@State(Scope.Benchmark)
class EvalscriptElseIfCalculations {
  val purchasecount: String = "purchaseCount"
  val multiplier: String = "multiplier"
  val price: String = "price"

  def params(purchaseCount: Int, price: Int): Map[String, _] = Map(this.purchasecount -> purchaseCount,this.price -> price)

  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) +  "\n$price = $multiplier*$price"

  def elseif(n: Int, p: String, c: String) = {
    var s: String = s"if ($$$p<1)\n  $$$c = $$$p+1\n"
    s += (2 to n - 1).map(x => s"else($$$p<$x)\n $$$c = $$$p+$x").mkString("\n")
    s += s"\nelse\n $$$c = $$$p+$n"
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

  def apply(s: Script, p: Map[String, _]) = {
    Interpreter.process(s, new GlobalContext(p))
  }

  def template(s: String): Script =  EvalScriptParser.load(s)


  @Benchmark
  def if_else_10(): Unit = {
    apply(t10, params10)
  }

  @Benchmark
  def if_else_100(): Unit = {
    apply(t100, params100)
  }
//
  @Benchmark
  def if_else_1000(): Unit = {
    apply(t1000, params1000)
  }
}
