package com.ividen.evalscript

import org.scalatest._

abstract class ExpressionSpec extends FlatSpec with Matchers {
  self: ScriptExecutor =>
    "Add/Minus" should "correctly processed" in {
      val result = executeScript(
        """
          |var i = 10
          |var j = 20
          |var s1="hello", s2='world'
          |var a1=[1,2,3], a2=[4,5,6,7]
          |$result_1 = i+j-1-2
          |$result_2 = i+j-(1-2)
          |$result_3 = s1 + s2
          |$result_4 = a1 + a2
        """.stripMargin)
      result should contain("result_4" -> Vector(1,2,3,4,5,6,7))
    }

    "Multiply/Device/Mod" should "correctly processed" in {
      val result = executeScript(
        """
          |var i = 10
          |var j = 20
          |s = "1"
          |$result_1 = i*j
          |$result_2 = j/i
          |$result_3 = i%6
          |$result_4 = s*4
        """.stripMargin)
      result should contain("result_1" -> BigDecimal(200))
      result should contain("result_2" -> BigDecimal(2))
      result should contain("result_3" -> BigDecimal(4))
      result should contain("result_4" -> "1111")
    }

    "Shift operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i = 10
          |var j = 20
          |$result_1 = i>>1
          |$result_2 = j<<2
        """.stripMargin)
      result should contain("result_1" -> BigDecimal(10 >> 1))
      result should contain("result_2" -> BigDecimal(20 << 2))
    }


    "Unary operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i = 10
          |var j = 20, b=true
          |$result_1 = -i
          |$result_2 = -$result_1
          |$result_3 = +i
          |$result_4 = ~j
          |$result_5 = !b
          |$result_6 = ~$result_5
        """.stripMargin)
      result should contain("result_1" -> BigDecimal(-10))
      result should contain("result_2" -> BigDecimal(10))
      result should contain("result_3" -> BigDecimal(10))
      result should contain("result_4" -> BigDecimal(~20))
      result should contain("result_5" -> false)
      result should contain("result_6" -> true)
    }

    "Increment operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i = 10
          |var j = 20
          |$result_1 = i++
          |$result_2 = i
          |$result_3 = ++i
          |$result_4 = i
          |$result_5 = j--
          |$result_6 = j
          |$result_7 = --j
          |$result_8 = j
        """.stripMargin)
      result should contain("result_1" -> BigDecimal(10))
      result should contain("result_2" -> BigDecimal(11))
      result should contain("result_3" -> BigDecimal(12))
      result should contain("result_4" -> BigDecimal(12))
      result should contain("result_5" -> BigDecimal(20))
      result should contain("result_6" -> BigDecimal(19))
      result should contain("result_7" -> BigDecimal(18))
      result should contain("result_8" -> BigDecimal(18))
    }

    "Bitwise operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i1 = 1, i2=2, i3=true, i4=false
          |
          |$result_1 = i1&i2
          |$result_2 = i1&(i2+2)
          |$result_3 = i1|i2
          |$result_4 = i1^i2
          |$result_5 = i3&i4
          |$result_6 = i3|i4
          |$result_7 = i3^i4
        """.stripMargin)

      result should contain("result_1" -> BigDecimal(1 & 2))
      result should contain("result_2" -> BigDecimal(1 & (2 + 2)))
      result should contain("result_3" -> BigDecimal(1 | 2))
      result should contain("result_4" -> BigDecimal(1 ^ 2))
      result should contain("result_5" -> false)
      result should contain("result_6" -> true)
      result should contain("result_7" -> true)
    }

    "Logical operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i1 = 1, i2=2
          |
          |$result_1 = (i1<i2) && true
          |$result_2 = (i1<i2) && false
          |$result_3 = (i1<i2) || true
          |$result_4 = (i1>i2) || false
          |
        """.stripMargin)

      result should contain("result_1" -> true)
      result should contain("result_2" -> false)
      result should contain("result_3" -> true)
      result should contain("result_4" -> false)
    }


    "Compare operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i1 = 1, i2=2, s1 = "abcd" ,s2="accd", b1=true,b2=false
          |
          |$result_1 = (i1<i2)
          |$result_2 = (i1>i2)
          |$result_3 = i1<=i2
          |$result_4 = (i1+1)<=i2
          |$result_5 = i1>=i2
          |$result_6 = (i1+1)>=i2
          |$result_7 = i1==i2
          |$result_8 = i1!=i2
          |$result_9 = s1<s2
          |$result_10= s1>s2
          |$result_11 = s1<=s2
          |$result_12 = s1<='abcd'
          |$result_13 = s1>=s2
          |$result_14 = s1>='abcd'
          |$result_15 = s1==s2
          |$result_16 = s1!=s2
          |$result_17 = s1<s2
          |$result_18 = s1>s2
          |
          |$result_19= s1>s2
          |$result_20 = s1<=s2
          |$result_21 = s1<='abcd'
          |$result_22 = s1>=s2
          |$result_23 = s1>='abcd'
          |$result_24 = s1==s2
          |$result_25 = s1!=s2
          |$result_26 = s1<s2
          |$result_27 = s1>s2
          |
          |
        """.stripMargin)

      result should contain("result_1" -> true)
      result should contain("result_2" -> false)
      result should contain("result_3" -> true)
      result should contain("result_4" -> true)
      result should contain("result_5" -> false)
      result should contain("result_6" -> true)
      result should contain("result_7" -> false)
      result should contain("result_8" -> true)
      result should contain("result_9" -> true)
      result should contain("result_10" -> false)
      result should contain("result_11" -> true)
      result should contain("result_12" -> true)
      result should contain("result_13" -> false)
      result should contain("result_14" -> true)
      result should contain("result_15" -> false)
      result should contain("result_16" -> true)
      result should contain("result_17" -> true)
      result should contain("result_18" -> false)

      result should contain("result_19" -> false)
      result should contain("result_20" -> true)
      result should contain("result_21" -> true)
      result should contain("result_22" -> false)
      result should contain("result_23" -> true)
      result should contain("result_24" -> false)
      result should contain("result_25" -> true)
      result should contain("result_26" -> true)
      result should contain("result_27" -> false)
    }


    "Index operations" should "correctly processed" in {
      val result = executeScript(
        """
          |var i1 = [1,2,3,"Test"]
          |
          |$result_1 = i1
          |$result_2 = i1[0]
          |$result_3 = i1[len(i1)-1]
          |$result_4 = $result_3[len($result_3)-1]
          |
          |i2 = [1,2,3,[4,5,6],7,8,[9]]
          |$result_5 = i2
          |$result_6 = i2[3]
          |$result_7 = $result_6[2]
          |
        """.stripMargin)

      result should contain("result_1" -> Vector(1, 2, 3, "Test"))
      result should contain("result_2" -> 1)
      result should contain("result_3" -> "Test")
      result should contain("result_4" -> "t")
      result should contain("result_5" -> Vector(1, 2, 3, Vector(4, 5, 6), 7, 8, Vector(9)))
      result should contain("result_6" -> Vector(4, 5, 6))
      result should contain("result_7" -> 6)
    }

  "Operation precedence " should "correctly processed" in {
    val result = executeScript(
      """
        |$result_1 = 1+2-20/4*5%6 + 8^16<<1|4>>2&7
        |$result_2 = (1+2-20)/(4*5)%6
        |$result_3 = (8^16)<<1|4>>(2&7)
        |$result_4 = (1+2-20)/(4*5)%6 + (8^16)<<1|4>>(2&7)
      """.stripMargin)
    result should contain("result_1" -> (1 + 2 - 20 / 4 * 5 % 6 + 8 ^ 16 << 1 | 4 >> 2 & 7))
    result should contain("result_2" -> BigDecimal(((1d + 2d - 20d) / (4d * 5d) % 6d)))
    result should contain("result_3" -> BigDecimal((8 ^ 16) << 1 | 4 >> (2 & 7)))
    //!!! attention we create "javascriptish" language with the same operation precendence. You can check with this:
    // <script language="javascript">
    //    alert((1+2-20)/(4*5)%6 + (8^16)<<1|4>>(2&7))
    //</script>
    result should contain("result_4" -> 47)
  }

  "Dynamic variable typing" should "processed correctly" in {
    val result = executeScript(
      """
        |var i = 1, s = "2", b = true , a=[1,2,3]
        |
        |$result_1 = i + s
        |$result_2 = s + i
        |$result_3 = i + b
        |$result_5 = s + b
        |$result_6 = a + i + s + b
        |
      """.stripMargin)

    result should contain("result_1" -> 3)
    result should contain("result_2" -> "21")
    result should contain("result_3" -> 2)
    result should contain("result_5" -> "2true")
    result should contain("result_6" -> Vector(1,2,3,1,"2",true))
  }

}

class InterpreterExpressionSpec extends ExpressionSpec with InterpretedExecutor
class CompiledExpressionSpec extends ExpressionSpec with CompiledExecutor

