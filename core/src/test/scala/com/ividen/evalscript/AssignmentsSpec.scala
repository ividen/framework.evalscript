package com.ividen.evalscript

import org.scalatest._

abstract class AssignmentsSpec extends FlatSpec with Matchers {
  self: ScriptExecutor =>
  "Assigments operations" should "correctly processed" in {
    val globals = Map("result_1"->1
      ,"result_2" ->2
      ,"result_3" ->6
      ,"result_4" ->4
      ,"result_5" ->5
      ,"result_6" ->6
      ,"result_7" ->7
      ,"result_8" ->8
      ,"result_9" ->9
      ,"result_10" ->81
      ,"result_11" ->8
      ,"result_12" ->12
      ,"result_13" ->14
      ,"result_14" ->15
      ,"result_15" ->17
      ,"result_16" ->true
      ,"result_17" ->false
      ,"result_18" ->2
      ,"result_19" ->"hello"
      ,"result_20" -> Vector(4,5,6,7)
      ,"result_21" -> "result_21"
    )
//    val result = executeScript(
//      """
//        |$result_1 += 1
//        |$result_2 -= 3
//        |$result_3 /= 0.5
//        |$result_4 *= 1.5
//        |$result_5 %= 2
//        |$result_6 >>= 1
//        |$result_7 <<= 2
//        |$result_8 ~= 10
//        |$result_9 &= 8
//        |$result_10 |=9
//        |$result_11 != 8
//        |$result_12++
//        |$result_13--
//        |++$result_14
//        |--$result_15
//        |$result_16 &&= false
//        |$result_17 ||= true
//        |$result_18 ^= 10
//        |$result_19 += "world"
//        |$result_20 += [1,2,3]
//        |$result_21 *=2
//      """.stripMargin, globals )


    val result = executeScript(
      """
        |$result_1 += 1
        |$result_2 -= 3
        |$result_3 /= 0.5
        |$result_4 *= 15
      """.stripMargin, globals )


    result should contain("result_1" -> 2)
    result should contain("result_2" -> -1)
    result should contain("result_3" -> 12)
    result should contain("result_4" -> 6)
    result should contain("result_5" -> 1)
    result should contain("result_6" -> (6 >> 1))
    result should contain("result_7" -> (7 << 2))
    result should contain("result_8" -> ~10)
    result should contain("result_9" -> (9 & 8))
    result should contain("result_10" -> (81 | 9))
    result should contain("result_11" -> false)
    result should contain("result_12" -> 13)
    result should contain("result_13" -> 13)
    result should contain("result_14" -> 16)
    result should contain("result_15" -> 16)
    result should contain("result_16" -> false)
    result should contain("result_17" -> true)
    result should contain("result_18" -> (2 ^ 10))
    result should contain("result_19" -> "helloworld")
    result should contain("result_20" -> Vector(4,5,6,7,1,2,3))
    result should contain("result_21" -> "result_21result_21")
  }

}

class InterpreterAssigmentsSpec extends AssignmentsSpec with InterpretedExecutor
class CompiledAssigmentsSpec extends AssignmentsSpec with CompiledExecutor
