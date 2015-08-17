package com.ividen.evalscript

import org.scalatest.{Matchers, FlatSpec}

abstract class CycleSpec  extends FlatSpec with Matchers {
  self: ScriptExecutor =>

  "While/Do" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |while($i<10){
        |  $result++
        |  $i++
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 6))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "While/Do with break" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |while(true){
        |  $result++
        |  $i++
        |  if($i>=10) break
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result" -> 6)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "While/Do with continue" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |while($i<10){
        |  if($i==7){
        |    $i++
        |    continue
        |  }
        |  $result++
        |  $i++
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 5))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "do/While" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |do{
        |  $result++
        |  $i++
        |}while($i<10)
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 6))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "do/While with break" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |do{
        |  $result++
        |  $i++
        |  if($i>=10) break
        |}while(true)
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result" -> 6)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "do/While with continue" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |do{
        |  if($i==7){
        |    $i++
        |    continue
        |  }
        |  $result++
        |  $i++
        |}while($i<10)
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 5))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "for" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |for(var j = $i; j<10; j++){
        |  $result++
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 6))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "For with break" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |for(j = $i; true;j++){
        |  if(j>=10) break
        |  $result++
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result" -> 6)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "For with continue" should "correctly processed" in {
    val s =
      """
        |$result = 0
        |for(var j = $i;j<10;j++){
        |  if(j==7){
        |    continue
        |  }
        |  $result++
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 5))
    result1 should contain("result" -> 4)
    result1 should contain("finished" -> true)
    result1 should have size 3
  }

  "Inner loops  with break" should "correctly processed" in {
    val s =
      """
        |$result_1 = 0
        |$result_2 = 0
        |
        |for(i = $i; i<10;i++){
        |   if(i==8) continue
        |   $result_1++
        |   for(j=$j;j<10;j++){
        |     if(i==j) break
        |     println("i="+i + ", j=" + j)
        |     $result_2++
        |   }
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4,"j"->2))
    println(result1)
    result1 should contain("result_1" -> 5)
    result1 should contain("result_2" -> 21)
    result1 should contain("finished" -> true)
    result1 should have size 5
  }



}

class InterpreterCycleSpec extends CycleSpec with InterpretedExecutor
class CompiledCycleSpec extends CycleSpec with CompiledExecutor

