package com.ividen.evalscript

import java.io.{FileOutputStream, File}
import java.security.MessageDigest

import org.apache.commons.codec.binary.{Hex, Base64, StringUtils}
import org.objectweb.asm.tree._
import org.objectweb.asm.{MethodVisitor, ClassWriter}
import org.objectweb.asm.Opcodes._

import scala.annotation.tailrec
import scala.collection
import scala.collection.parallel.mutable


abstract class CompiledScript(val globals: GlobalContext) {
  def execute
}



class DerivedCompiledScript(g: GlobalContext) extends CompiledScript(g) {

  override def execute: Unit ={
    val literal1 = DecimalLiteral(1)
    val literal2  = DecimalLiteral(1)

    val literal3 = literal1 + literal2
    println(literal3)
  }
}


object ScriptCompiler {

  private class Generator(b: `{}`, name: String) extends ClassLoader{
    val cn = new ClassNode(ASM4)
    val exec = new MethodNode(ACC_PUBLIC, "execute", "()V", null, Array.empty)
    val methods: java.util.List[MethodNode] = cn.methods.asInstanceOf[java.util.List[MethodNode]]
    val fields: java.util.List[FieldNode] = cn.fields.asInstanceOf[java.util.List[FieldNode]]
    var fieldNumber:Int = 0
    val literals: collection.mutable.Map[Literal, String] = new collection.mutable.HashMap[Literal,String]()

    def compile : Class[CompiledScript] = {
      initClassName
      generateConstructor
      methods.add(exec)
      exec.instructions.add(new InsnNode(RETURN))
      b.items.foreach(processElement)
      initStaticVars

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cn.accept(cw)
      val result =cw.toByteArray
      val file = new File("/Users/alexander.guzanov/prj/Test.class")
      file.createNewFile()
      val stream = new FileOutputStream(file, false)
      stream.write(result)
      stream.close()

      defineClass(name,result,0,result.length).asInstanceOf[Class[CompiledScript]]
    }

    def initStaticVars = {
      if(!literals.isEmpty){
        val m = new MethodNode(ACC_STATIC,"{}", "()V",null,Array.empty)
        for(e <- literals){
          e._1 match {
            case DecimalLiteral(v) =>
            case StringLiteral(v) =>
            case BooleanLiteral(v) =>
          }
        }
      }
    }

    private def initClassName: Unit = {
      cn.version = V1_6
      cn.access = ACC_PUBLIC
      cn.name = name
      cn.superName = "com/ividen/evalscript/CompiledScript"
    }

    private def generateConstructor: Unit = {
      val constructor = new MethodNode(ACC_PUBLIC, "<init>", "(Lcom/ividen/evalscript/GlobalContext;)V", null, Array.empty)
      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitVarInsn(ALOAD, 1)
      constructor.visitMethodInsn(INVOKESPECIAL, "com/ividen/evalscript/CompiledScript", "<init>", "(Lcom/ividen/evalscript/GlobalContext;)V")
      constructor.visitInsn(RETURN)
      constructor.visitMaxs(1, 1)
      constructor.visitEnd()
      methods.add(constructor)
    }

    private def processElement(e: ScriptElement): Unit = e match {
      case exp: Expression => processExpression(exp)
      //    case DeclareVars(l) => l.foreach(declareVar)
      //    case assignment: `=` => processAssignment(assignment)
      //    case `if else`(i, e) => processIfElse(i, e)
      //    case `while do`(e, b,p) => processWhileDo(e, b,p)
      //    case `do while`(e, b) => processDoWhile(e, b)
      //    case `switch`(e, c, d) => processSwitch(e, c, d)
      //    case _: `break` => this.break
      //    case _: `continue` => this.breakContinue
      //    case b: `{}` => processNewBlock(b)
    }

    private def createField(l: Literal) = {
      fieldNumber += 1
      val n = s"v$fieldNumber"
      val node = new FieldNode(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, s"v$fieldNumber", "Lcom/ividen/evalscript/Literal;", null, null)
      fields.add(node)
      literals += l -> node.name
      node.name
    }

    private def processExpression(e: Expression):Unit = e match {
      case LiteralExpression(l: Literal) => {
        var name = literals.getOrElse(l, createField(l))

      }
      case `:+`(l, r) => {
        processExpression(l)
        processExpression(r)
        //invoke special
      }
      //    case `:-`(l, r) => processExpression(l) - processExpression(r)
      //    case `/`(l, r) => processExpression(l) / processExpression(r)
      //    case `*`(l, r) => processExpression(l) * processExpression(r)
      //    case `%`(l, r) => processExpression(l) % processExpression(r)
      //    case `!:`(r) => !processExpression(r)
      //    case `[]`(l,r) => processExpression(l).apply(processExpression(r))
      //    case `~:`(r) => ~processExpression(r)
      //    case `-:`(r) => -processExpression(r)
      //    case `+:`(r) => +processExpression(r)
      //    case `++:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      //    case `--:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      //    case `:++`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result + DecimalLiteral(BigDecimal(1))))); result
      //    case `:--`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result - DecimalLiteral(BigDecimal(1))))); result
      //    case `>>`(l, r) => processExpression(l) >> processExpression(r)
      //    case `<<`(l, r) => processExpression(l) << processExpression(r)
      //    case `&`(l, r) => processExpression(l) & processExpression(r)
      //    case `^`(l, r) => processExpression(l) ^ processExpression(r)
      //    case `|`(l, r) => processExpression(l) | processExpression(r)
      //    case `&&`(l, r) => processExpression(l) && processExpression(r)
      //    case `||`(l, r) => processExpression(l) || processExpression(r)
      //    case `:==`(l, r) => processExpression(l) == processExpression(r)
      //    case `:!=`(l, r) => processExpression(l) != processExpression(r)
      //    case `<`(l, r) => processExpression(l) < processExpression(r)
      //    case `>`(l, r) => processExpression(l) > processExpression(r)
      //    case `>=`(l, r) => processExpression(l) >= processExpression(r)
      //    case `<=`(l, r) => processExpression(l) <= processExpression(r)
      //    case GerVar(v: LocalVariable) => localContext(v)
      //    case GerVar(v: GlobalVairable) => globalContext(v)
      //    case `call`(n,a) => FunctionInvoker.invoke(n,a.map(x => processExpression(mv,x)))
    }

  }
  def compile(s: Script) = new Generator(s.block,generateClassName(s)).compile

  private def genConstructor(cw: ClassWriter) = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "(Lcom/ividen/evalscript/GlobalContext;)V", null, null)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKESPECIAL, "com/ividen/evalscript/CompiledScript", "<init>", "(Lcom/ividen/evalscript/GlobalContext;)V")
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }
  private def generateClassName(s: Script): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(s.toString.getBytes)

    s"G${Hex.encodeHexString(digest)}"
  }


  def main(args: Array[String]) {
    val s = EvalScriptParser.load("1+1")
    val cs = compile(s)
    cs.getConstructor(classOf[GlobalContext]).newInstance(new GlobalContext(Map.empty[String, Any])).execute
  }
}
