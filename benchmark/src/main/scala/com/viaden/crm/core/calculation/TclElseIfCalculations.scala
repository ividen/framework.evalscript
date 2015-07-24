package com.viaden.crm.core.calculation

import java.io.{ByteArrayOutputStream, OutputStreamWriter, StringReader}

import freemarker.template.{Configuration, Template}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import tcl.lang.{TclException, TCL, Interp}

import scala.collection.mutable


//@State(Scope.Benchmark)
//class TclElseIfCalculations {
//  val purchasecount: String = "purchaseCount"
//  val multiplier: String = "multiplier"
//  val price: String = "price"
//
//  def params(purchaseCount: Int, price: Int): mutable.Map[String, Int] = {
//    val result: mutable.Map[String, Int] = mutable.Map()
//    result += this.purchasecount -> purchaseCount
//    result += this.price -> price
//    result
//  }
//
//  def settings(c: Int, p: Int): String = elseif(c, purchasecount, multiplier) +  "\nset price [expr $multiplier * $price] "
//
//  def elseif(n: Int, p: String, c: String) = {
//
//    var s: String = s"if {$$$p < 1 } {\n set $c [expr[ $$$p + 1 ] ]\n}"
//    s += (2 to n - 1).map(x => s" elseif {$$$p < $x} {\n set $c [expr $$$p + 1]\n}").mkString("")
//    s += s" else {\n set $c [expr $$$p + $n] \n}"
//    s
//  }
//
//  val s10 = settings(10, 10)
//  val s100 =settings(100, 10)
//  val s1000 =settings(1000, 10)
//
//  val t10 = template(s10)
//  val t100 = template(s100)
//  val t1000 = template(s1000)
//
//  val params10 = params(10, 10)
//  val params100 = params(100, 10)
//  val params1000 = params(1000, 10)
//
//
//  def apply(s: String, params: mutable.Map[String, Int]) = {
//    val interp: Interp = new Interp
//
//    params.foreach((x) => {
//      interp.setVar(x._1, null, x._2, TCL.GLOBAL_ONLY)
//    })
//    interp.eval(s)
//    interp.dispose()
//  }
//
//  def template(settings: String): Template = {
//    new Template("name", new StringReader(settings), new Configuration(Configuration.VERSION_2_3_22))
//  }
//
////  @Benchmark
////  def if_else_10(): Unit = {
////    apply(s10, params10)
////  }
////
////  @Benchmark
////  def if_else_100(): Unit = {
////    apply(s100, params100)
////  }
//
//  @Benchmark
//  def if_else_1000(): Unit = {
//    apply(s1000, params1000)
//  }
//}
