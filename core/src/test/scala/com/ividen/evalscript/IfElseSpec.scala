package com.ividen.evalscript

import org.scalatest.{Matchers, FlatSpec}

abstract class IfElseSpec extends FlatSpec with Matchers {
  self: ScriptExecutor =>

  "If without else " should "correctly processed" in {
    val s =
      """
        |if($i<10){
        |  $result = true
        |}
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> true)
    result1 should have size 2
    val result2 = executeScript(s, Map("i" -> 20))
    result2 should have size 1
  }

  "If with else " should "correctly processed" in {
    val s =
      """
        |if($i<10){
        |  $result = true
        |} else{
        |  $result = false
        |}
        |
        |$finished = true
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> true)
    result1 should contain("finished" -> true)
    result1 should have size 3
    val result2 = executeScript(s, Map("i" -> 20))
    result2 should contain("result" -> false)
    result2 should contain("finished" -> true)
    result2 should have size 3
  }


  "If with else without {}" should "correctly processed" in {
    val s =
      """
        |if($i<10)
        |  $result = true
        |else
        |  $result = false
        |
        |$finished = true
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> true)
    result1 should contain("finished" -> true)
    result1 should have size 3
    val result2 = executeScript(s, Map("i" -> 20))
    result2 should contain("result" -> false)
    result2 should contain("finished" -> true)
    result2 should have size 3
  }


  "If with some else " should "correctly processed" in {
    val s =
      """
        |if($i<10){
        |  $result = "0<x<10"
        |}else($i<20) {
        |  $result = "10<=x<20"
        |}else($i<30){
        |  $result = "20<=x<30"
        |}else{
        |  $result = ">=30"
        |}
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> "0<x<10")
    result1 should contain("finished" -> true)
    result1 should have size 3

    val result2 = executeScript(s, Map("i" -> 11))
    result2 should contain("result" -> "10<=x<20")
    result2 should contain("finished" -> true)
    result2 should have size 3

    val result3 = executeScript(s, Map("i" -> 21))
    result3 should contain("result" -> "20<=x<30")
    result3 should contain("finished" -> true)
    result3 should have size 3

    val result4 = executeScript(s, Map("i" -> 31))
    result4 should contain("result" -> ">=30")
    result4 should contain("finished" -> true)
    result4 should have size 3
  }

  "If with some else(condition) and without default else " should "correctly processed" in {
    val s =
      """
        |if($i<10){
        |  $result = "0<x<10"
        |}else($i<20){
        |  $result = "10<=x<20"
        |}else($i<30){
        |  $result = "20<=x<30"
        |}
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> "0<x<10")
    result1 should contain("finished" -> true)
    result1 should have size  3

    val result2 = executeScript(s, Map("i" -> 11))
    result2 should contain("result" -> "10<=x<20")
    result2 should contain("finished" -> true)
    result2 should have size  3

    val result3 = executeScript(s, Map("i" -> 21))
    result3 should contain("result" -> "20<=x<30")
    result3 should contain("finished" -> true)
    result3 should have size  3

    val result4 = executeScript(s, Map("i" -> 31))
    result4 should have size 2
    result4 should contain("finished" -> true)
  }

  "Some inner ifs" should "correctly processed" in {
//    val s =
//      """
//        |if($i<10){
//        |  $result = "0<x<10"
//        |  if($i>2){
//        |    $result += "and >2"
//        |  }
//        |}else($i<20){
//        |  $result = "10<=x<20"
//        |}
//        |$finished = true
//        |
//      """.stripMargin



    val s =
      """
        |if($i<10){
        |  $result = "0<x<10"
        |  if($i>2){
        |    $result += "and >2"
        |  }else{
        |    $result += "and <=2"
        |  }
        |}else($i<20){
        |  $result = "10<=x<20"
        |  if($i>12){
        |    $result += "and >12"
        |  }else{
        |    $result += "and <=12"
        |  }
        |}else($i<30){
        |  $result = "20<=x<30"
        |  if($i>22){
        |    $result += "and >22"
        |  }else{
        |    $result += "and <=22"
        |  }
        |}
        |$finished = true
        |
      """.stripMargin


    val result1 = executeScript(s, Map("i" -> 1))
    result1 should contain("result" -> "0<x<10and <=2")
    result1 should contain("finished" -> true)
    result1 should have size  3

    val result1_1 = executeScript(s, Map("i" -> 4))
    result1_1 should contain("result" -> "0<x<10and >2")
    result1_1 should contain("finished" -> true)
    result1_1 should have size  3

    val result2 = executeScript(s, Map("i" -> 11))
    result2 should contain("result" -> "10<=x<20and <=12")
    result2 should contain("finished" -> true)
    result2 should have size  3

    val result2_1 = executeScript(s, Map("i" -> 13))
    result2_1 should contain("result" -> "10<=x<20and >12")
    result2_1 should contain("finished" -> true)
    result2_1 should have size  3

    val result3 = executeScript(s, Map("i" -> 21))
    result3 should contain("result" -> "20<=x<30and <=22")
    result3 should contain("finished" -> true)
    result3 should have size  3

    val result3_1 = executeScript(s, Map("i" -> 25))
    result3_1 should contain("result" -> "20<=x<30and >22")
    result3_1 should contain("finished" -> true)
    result3_1 should have size  3

    val result4 = executeScript(s, Map("i" -> 31))
    result4 should have size 2
    result4 should contain("finished" -> true)
  }

}

class InterpreterIfElseSpec extends IfElseSpec with InterpretedExecutor

class CompiledIfElseSpec extends IfElseSpec with CompiledExecutor
