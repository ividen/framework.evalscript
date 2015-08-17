package com.ividen.evalscript

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by alexander.guzanov on 8/17/15.
 */
abstract class SwitchSpec extends FlatSpec with Matchers {
  self: ScriptExecutor =>

  "Switch with one case" should "correctly processed" in {
    val s =
      """
        |switch($i+1){
        |  case 5: $result = 5
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result" -> 5)
    result1 should contain("finished" -> true)
    result1 should have size 3
    val result2 = executeScript(s, Map("i" -> 20))
    result1 should contain("finished" -> true)
    result2 should have size 2
  }

  "Switch with cases and without break" should "correctly processed" in {
    val s =
      """
        |switch($i+1){
        |  case 5: $result_5 = 5
        |  case 10: $result_10 = 10
        |  default: $result_def = "default"
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result_5" -> 5)
    result1 should contain("result_10" -> 10)
    result1 should contain("result_def" -> "default")
    result1 should contain("finished" -> true)
    result1 should have size 5
    val result2 = executeScript(s, Map("i" -> 9))
    result2 should contain("finished" -> true)
    result2 should contain("result_10" -> 10)
    result2 should contain("result_def" -> "default")
    result2 should have size 4

    val result3 = executeScript(s, Map("i" -> 40))
    result3 should contain("finished" -> true)
    result3 should contain("result_def" -> "default")
    result3 should have size 3
  }

  "Switch with cases and with a break" should "correctly processed" in {
    val s =
      """
        |switch($i+1){
        |  case 5: $result_5 = 5
        |  case 10: {
        |      $result_10 = 10
        |      break
        |    }
        |  default: $result_def = "default"
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result_5" -> 5)
    result1 should contain("result_10" -> 10)
    result1 should contain("finished" -> true)
    result1 should have size 4
    val result2 = executeScript(s, Map("i" -> 9))
    result2 should contain("finished" -> true)
    result2 should contain("result_10" -> 10)
    result2 should have size 3

    val result3 = executeScript(s, Map("i" -> 40))
    result3 should contain("finished" -> true)
    result3 should contain("result_def" -> "default")
    result3 should have size 3
  }

  "Switch with cases and with  breaks" should "correctly processed" in {
    val s =
      """
        |switch($i+1){
        |  case 5: {
        |      $result_5 = 5
        |      break
        |    }
        |  case 10: {
        |      $result_10 = 10
        |      break
        |    }
        |  default: $result_def = "default"
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result_5" -> 5)
    result1 should contain("finished" -> true)
    result1 should have size 3
    val result2 = executeScript(s, Map("i" -> 9))
    result2 should contain("finished" -> true)
    result2 should contain("result_10" -> 10)
    result2 should have size 3

    val result3 = executeScript(s, Map("i" -> 40))
    result3 should contain("finished" -> true)
    result3 should contain("result_def" -> "default")
    result3 should have size 3
  }

  "Switch with cases and with  breaks and without {}" should "correctly processed" in {
    val s =
      """
        |switch($i+1){
        |  case 5:
        |      $result_5 = 5
        |      break
        |  case 10:
        |      $result_10 = 10
        |      break
        |  default: $result_def = "default"
        |}
        |
        |$finished = true
        |
      """.stripMargin

    val result1 = executeScript(s, Map("i" -> 4))
    result1 should contain("result_5" -> 5)
    result1 should contain("finished" -> true)
    result1 should have size 3
    val result2 = executeScript(s, Map("i" -> 9))
    result2 should contain("finished" -> true)
    result2 should contain("result_10" -> 10)
    result2 should have size 3

    val result3 = executeScript(s, Map("i" -> 40))
    result3 should contain("finished" -> true)
    result3 should contain("result_def" -> "default")
    result3 should have size 3
  }
}

class InterpreterSwitchSpec extends SwitchSpec with InterpretedExecutor

class CompiledSwitchSpec extends SwitchSpec with CompiledExecutor

