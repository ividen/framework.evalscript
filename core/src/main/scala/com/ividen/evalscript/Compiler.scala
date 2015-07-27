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
import scala.reflect.ClassTag


abstract class CompiledScript(val globals: GlobalContext) {
  def execute

  protected def setGlobal(n: String, v: Literal) = globals.set(n, v)
  protected def getGlobal(n: String): Literal = globals(n)
  protected def checkCondition(l: Literal): Boolean = l match {
    case DecimalLiteral(x) if x != 0 => true
    case StringLiteral(x) if !x.isEmpty => true
    case BooleanLiteral(x) if x => true
    case _ => false
  }
}

class DerivedCompiledScript(g: GlobalContext) extends CompiledScript(g) {

  override def execute: Unit = {
    val literal1 = DecimalLiteral(1)
    val literal2 = DecimalLiteral(1)

    val literal3 = literal1 + literal2
    println(literal3)
  }
}

object Generator {
  private val mapping: Map[Class[_ <: Expression], String] = Map[Class[_ <: Expression], String](
    classOf[`[]`] -> "apply", classOf[`:+`] -> "$plus", classOf[`:-`] -> "$minus", classOf[`/`] -> "$div", classOf[`*`] -> "$times", classOf[`%`] -> "$percent", classOf[`!:`] -> "unary_$bang"
    , classOf[`~:`] -> "unary_$tilde", classOf[`-:`] -> "unary_$minus", classOf[`+:`] -> "unary_$plus", classOf[`<<`] -> "$less$less", classOf[`>>`] -> "$greater$eq", classOf[`&`] -> "$amp"
    , classOf[`^`] -> "$up", classOf[`|`] -> "$bar", classOf[`&&`] -> "$amp$amp", classOf[`||`] -> "$bar$bar", classOf[`:==`] -> "$eq$eq", classOf[`:!=`] -> "$bang$eq", classOf[`<`] -> "$less"
    , classOf[`>`] -> "$greater", classOf[`>=`] -> "$greater$eq", classOf[`<=`] -> "$less$eq")
}

private class Generator(b: `{}`, name: String) extends ClassLoader {

  import Generator._

  private val cn = new ClassNode(ASM4)
  private val exec = new MethodNode(ACC_PUBLIC, "execute", "()V", null, Array.empty)
  private val methods: java.util.List[MethodNode] = cn.methods.asInstanceOf[java.util.List[MethodNode]]
  private val fields: java.util.List[FieldNode] = cn.fields.asInstanceOf[java.util.List[FieldNode]]
  private var fieldNumber: Int = 0
  private val literals: collection.mutable.Map[Literal, String] = new collection.mutable.HashMap[Literal, String]()
  private val globals: collection.mutable.Set[String] = new collection.mutable.HashSet[String]()

  def compile: Class[CompiledScript] = {
    initClassName
    methods.add(exec)
    b.items.foreach(processElement)
    addIns(new InsnNode(RETURN))
    initStaticVars
    generateConstructor

    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cn.accept(cw)
    val result = cw.toByteArray
    val file = new File("/Users/alexander.guzanov/prj/Test.class")
    file.createNewFile()
    val stream = new FileOutputStream(file, false)
    stream.write(result)
    stream.close()

    defineClass(name, result, 0, result.length).asInstanceOf[Class[CompiledScript]]
  }

  private def initStaticVars = {
    if (!literals.isEmpty) {
      val m = new MethodNode(ACC_STATIC, "<clinit>", "()V", null, Array.empty)
      for (e <- literals) {
        e._1 match {
          case DecimalLiteral(v) => initDecimal(m, e._2, v)
          case StringLiteral(v) => initString(m, e._2, v)
          case BooleanLiteral(v) => initBoolean(m, e._2, v)
        }
      }
      m.instructions.add(new InsnNode(RETURN))
      methods.add(m)
    }
  }

  private def addIns(il: AbstractInsnNode*) = il.foreach(exec.instructions.add)
  private def addIns(m: MethodNode, il: AbstractInsnNode*) = il.foreach(m.instructions.add)

  private def initDecimal(m: MethodNode, n: String, v: BigDecimal): Unit = addIns(m, new TypeInsnNode(NEW, typeString[DecimalLiteral]), new InsnNode(DUP)
    , new LdcInsnNode(v.longValue()), new MethodInsnNode(INVOKESTATIC, typeString[BigDecimal], "long2bigDecimal", s"(J)${typeSignature[BigDecimal]}")
    , new MethodInsnNode(INVOKESPECIAL, typeString[DecimalLiteral], "<init>", s"(${typeSignature[BigDecimal]})V"), new FieldInsnNode(PUTSTATIC, cn.name, n, typeSignature[Literal]))

  private def initString(m: MethodNode, n: String, v: String): Unit = addIns(m, new TypeInsnNode(NEW, typeString[StringLiteral]), new InsnNode(DUP)
    , new LdcInsnNode(v), new MethodInsnNode(INVOKESPECIAL, typeString[StringLiteral], "<init>", s"(${typeSignature[String]})V")
    , new FieldInsnNode(PUTSTATIC, cn.name, n, typeSignature[Literal]))

  private def initBoolean(m: MethodNode, n: String, v: Boolean) = addIns(m, new TypeInsnNode(NEW, typeString[BooleanLiteral])
    , new InsnNode(DUP)
    , new LdcInsnNode(v)
    , new MethodInsnNode(INVOKESPECIAL, typeString[BooleanLiteral], "<init>", s"(Z)V")
    , new FieldInsnNode(PUTSTATIC, cn.name, n, typeSignature[Literal]))

  private def initClassName: Unit = {
    cn.version = V1_6
    cn.access = ACC_PUBLIC
    cn.name = name
    cn.superName = typeString[CompiledScript]
  }


  private def typeSignature[T](implicit tag: ClassTag[T]) = "L" + typeString[T](tag) + ";"
  private def typeString[T](implicit tag: ClassTag[T]) = tag.runtimeClass.getName.replaceAll("\\.", "/")

  private def generateConstructor: Unit = {
    val constructor = new MethodNode(ACC_PUBLIC, "<init>", s"(${typeSignature[GlobalContext]})V", null, Array.empty)
    addIns(constructor,new VarInsnNode(ALOAD, 0))
    addIns(constructor,new VarInsnNode (ALOAD, 1))
    addIns(constructor,new MethodInsnNode(INVOKESPECIAL, typeString[CompiledScript], "<init>", s"(${typeSignature[GlobalContext]})V"))
    for(g <- globals){
      val node = new FieldNode(ACC_PUBLIC , g, typeSignature[Literal], null, null)
      fields.add(node)
//      addIns(constructor, new TypeInsnNode(NEW, typeString[StringLiteral]), new InsnNode(DUP)
//        , new LdcInsnNode(), new MethodInsnNode(INVOKESPECIAL, typeString[StringLiteral], "<init>", s"(${typeSignature[String]})V")
//        , new FieldInsnNode(PUTFIELD, cn.name, g, typeSignature[Literal]))

    }

    addIns(constructor,new InsnNode(RETURN))
    methods.add(constructor)
  }

  private def processElement(e: ScriptElement): Unit = e match {
    case exp: Expression => processExpression(exp)
    //    case DeclareVars(l) => l.foreach(declareVar)
    case assignment: `=` => processAssignment(assignment)
        case `if else`(i, e) => processIfElse(i, e)
    //    case `while do`(e, b,p) => processWhileDo(e, b,p)
    //    case `do while`(e, b) => processDoWhile(e, b)
    //    case `switch`(e, c, d) => processSwitch(e, c, d)
    //    case _: `break` => this.break
    //    case _: `continue` => this.breakContinue
//        case b: `{}` => processNewBlock(b)
        case b: `{}` => b.items.foreach(processElement)
  }

//
  private def processIfElse(_if: `if`, _else: Seq[`else`]) = {
    def checkCondition(c: Expression): Unit ={
      addIns(new VarInsnNode(ALOAD,0))
      processExpression(c)
      addIns(new MethodInsnNode(INVOKEVIRTUAL,cn.name, "checkCondition", s"(${typeSignature[Literal]})Z"))
    }
    val blockLabel = new LabelNode()
    var lastLabel = new LabelNode()
    checkCondition(_if.c)

    addIns(new JumpInsnNode(IFEQ,lastLabel))
    processElement(_if.block)
    addIns(new JumpInsnNode(GOTO,blockLabel))
    if(_else.isEmpty){
      addIns(lastLabel)
    }else for(e <- _else){
      addIns(lastLabel)
      for(c <- e.c) {
        lastLabel = new LabelNode()
        checkCondition(c)
        addIns(new JumpInsnNode(IFEQ, lastLabel))
      }
      processElement(e.block)
      addIns(new JumpInsnNode(GOTO,blockLabel))
    }

    addIns(blockLabel)

  }

  private def getFieldName(l: Literal) = literals.getOrElse(l, createField(l))

  private def createField(l: Literal) = {
    fieldNumber += 1
    val n = s"v$fieldNumber"
    val node = new FieldNode(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, s"v$fieldNumber", typeSignature[Literal], null, null)
    fields.add(node)
    literals += l -> node.name
    node.name
  }




  private def processExpression(e: Expression): Unit = {
    e match {
      case LiteralExpression(l: Literal) => fieldGet(l)
      case v: UnaryExpression => processExpression(v.r)
      case v: BinaryExpression => processExpression(v.l); processExpression(v.r); invokeOperator(v.getClass.asInstanceOf[Class[_ <: Expression]])
      //    case GerVar(v: LocalVariable) => localContext(v)
      case GerVar(v: GlobalVairable) => {

        if(!globals.contains(v.name)){
          globals += v.name
        }

        addIns(new VarInsnNode(ALOAD, 0), new FieldInsnNode(GETFIELD, cn.name, v.name, typeSignature[Literal]))

      }
      case `call`(n, a) => {
        addIns(new FieldInsnNode(GETSTATIC, typeString[Functions] + "$", "MODULE$", s"L${typeString[Functions]}$$;"))
        a.reverse.foreach(processExpression)
        addIns(new MethodInsnNode(INVOKEVIRTUAL, typeString[Functions] + "$", n, s"(${typeSignature[Literal]*a.length})${if(FunctionInvoker.isReturnLiteral(n))typeSignature[Literal]else "V"}"))
      }
      //      case `++:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      //      case `--:`(v) => val result = processExpression(GerVar(v)) + DecimalLiteral(BigDecimal(1)); processAssignment(`=`(v, LiteralExpression(result))); result
      //      case `:++`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result + DecimalLiteral(BigDecimal(1))))); result
      //      case `:--`(v) => val result = processExpression(GerVar(v)); processAssignment(`=`(v, LiteralExpression(result - DecimalLiteral(BigDecimal(1))))); result
    }
  }

  private def processAssignment(assignment: `=`) = (assignment.l, assignment.r) match {
    //    case (v: LocalVariable, e) => localContext.set(v, processExpression(e))
    case (g: GlobalVairable, e) => {
//      addIns(new VarInsnNode(ALOAD, 0), new LdcInsnNode(g.name))
      addIns(new VarInsnNode(ALOAD, 0))
      processExpression(e);
//      addIns(new MethodInsnNode(INVOKEVIRTUAL, cn.name, "setGlobal", s"(${typeSignature[String]}${typeSignature[Literal]})V"))
      addIns(new FieldInsnNode(PUTFIELD, cn.name, g.name, typeSignature[Literal]))
    }
  }

  private def processAssignment(assignment: `=`) = (assignment.l, assignment.r) match {
    //    case (v: LocalVariable, e) => localContext.set(v, processExpression(e))
    case (g: GlobalVairable, e) => addIns(new VarInsnNode(ALOAD, 0),new LdcInsnNode(g.name)); processExpression(e); addIns(new MethodInsnNode(INVOKEVIRTUAL, cn.name, "setGlobal", s"(${typeSignature[String]}${typeSignature[Literal]})V"))
  }

  private def invokeOperator(c: Class[_ <: Expression]) = addIns(new MethodInsnNode(INVOKEINTERFACE, typeString[Literal], mapping.getOrElse(c, ""), s"(${typeSignature[Literal]})${typeSignature[Literal]}"))
  private def fieldGet(l: Literal) = addIns(new FieldInsnNode(GETSTATIC, cn.name, getFieldName(l), typeSignature[Literal]))
}


object ScriptCompiler {

  def compile(s: Script) = new Generator(s.block, generateClassName(s)).compile

  private def generateClassName(s: Script): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(s.toString.getBytes)

    s"G${Hex.encodeHexString(digest)}"
  }


  def main(args: Array[String]) {
    val script = "$v=11 \n if($v==99) println(99) \n else($v==101) println(101) \n else println('else ' + $v)"
    println(script)
    val s = EvalScriptParser.load(script)
    println(script)
    val cs = compile(s)
    val ctx = new GlobalContext(Map.empty[String, Any])
    val instance = cs.getConstructor(classOf[GlobalContext]).newInstance(ctx)
    instance.execute
    println(ctx.vars)
  }
}

