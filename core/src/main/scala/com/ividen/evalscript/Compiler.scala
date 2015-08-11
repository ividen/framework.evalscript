package com.ividen.evalscript

import java.io.{FileOutputStream, File}
import java.security.MessageDigest

import org.apache.commons.codec.binary.{Hex, Base64, StringUtils}
import org.objectweb.asm.tree._
import org.objectweb.asm.{MethodVisitor, ClassWriter}
import org.objectweb.asm.Opcodes._
import scala.annotation.tailrec
import scala.collection
import scala.collection.mutable
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable
import scala.reflect.ClassTag


abstract class CompiledScript(globals: collection.immutable.Map[String, Literal]) {
  def execute

  def getGlobals: Map[String, Literal]

  protected def checkCondition(l: Literal): Boolean = l match {
    case DecimalLiteral(x) if x != 0 => true
    case StringLiteral(x) if !x.isEmpty => true
    case BooleanLiteral(x) if x => true
    case _ => false
  }

  protected def getGlobal(n: String, globals: Map[String, Literal]): Literal = globals.getOrElse(n, NullLiteral)
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

  case class BlockInfo(start: LabelNode, end: LabelNode, index: Int, prevBlock: Option[BlockInfo]) {
    private val localVarsMapping: collection.mutable.Map[String, Int] = new collection.mutable.HashMap[String, Int]

    def apply(n: String): Option[Int] = localVarsMapping.get(n).fold(prevBlock.flatMap(b => b.apply(n)))(Some(_))
    def define(n: String, index: Int): Unit = localVarsMapping += (n -> index)
  }

  private val cn = new ClassNode(ASM4)
  private val exec = new MethodNode(ACC_PUBLIC, "execute", "()V", null, Array.empty)
  private val methods: java.util.List[MethodNode] = cn.methods.asInstanceOf[java.util.List[MethodNode]]
  private val fields: java.util.List[FieldNode] = cn.fields.asInstanceOf[java.util.List[FieldNode]]
  private val literals: collection.mutable.Map[Literal, String] = new collection.mutable.HashMap[Literal, String]()
  private val globals: collection.mutable.Set[String] = new collection.mutable.HashSet[String]()
  private val localVars: java.util.List[LocalVariableNode] = exec.localVariables.asInstanceOf[java.util.List[LocalVariableNode]]

  private var staticFieldCounter: Int = 0
  private var localVariableCounter: Int = 0
  private var blockCounter: Int = 0


  def compile: Class[CompiledScript] = {
    initClassName
    methods.add(exec)
    processBlock(None, b)
    addIns(new InsnNode(RETURN))
    initStaticVars
    generateConstructor
    generateGetGlobals
    val result: Array[Byte] = acceptClass
    defineClass(name, result, 0, result.length).asInstanceOf[Class[CompiledScript]]
  }

  private def acceptClass: Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cn.accept(cw)
    val result = cw.toByteArray
    saveClass(result)
    result
  }

  private def saveClass(result: Array[Byte]): Unit = {
    val file = new File("/Users/alexander.guzanov/prj/TestClass.class")
    file.createNewFile()
    val stream = new FileOutputStream(file, false)
    stream.write(result)
    stream.close()
  }

  private def initStaticVars = {
    if (!literals.isEmpty) {
      val m = new MethodNode(ACC_STATIC, "<clinit>", "()V", null, Array.empty)
      for (e <- literals) {
        //todo aguzanov arrayliteral
        e._1 match {
          case DecimalLiteral(v) => initDecimal(m, e._2, v)
          case StringLiteral(v) => initString(m, e._2, v)
          case BooleanLiteral(v) => initBoolean(m, e._2, v)
          case ArrayLiteral(v) => println(v)
          case _ =>
        }
      }
      m.instructions.add(new InsnNode(RETURN))
      methods.add(m)
    }
  }


  private def processBlock(parentBlock: Option[BlockInfo],b: `{}`) = {
    val blockInfo = createBlockInfo(parentBlock)
    addIns(blockInfo.start)
    b.items.foreach(processElement(blockInfo, _))
    addIns(blockInfo.end)
  }

  private def createBlockInfo(parentBlock: Option[BlockInfo]): BlockInfo = BlockInfo(new LabelNode(), new LabelNode(), {
    blockCounter += 1;
    blockCounter
  }, parentBlock)
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

  private def initClassName: Unit = cn.visit(V1_5, ACC_PUBLIC, name, null, typeString[CompiledScript], Array.empty)
  private def typeSignature[T](implicit tag: ClassTag[T]) = "L" + typeString[T](tag) + ";"
  private def typeString[T](implicit tag: ClassTag[T]) = tag.runtimeClass.getName.replaceAll("\\.", "/")

  private def generateConstructor: Unit = {
    val constructor = new MethodNode(ACC_PUBLIC, "<init>", s"(${typeSignature[scala.collection.immutable.Map[_, _]]})V", null, Array.empty)
    addIns(constructor, new VarInsnNode(ALOAD, 0), new VarInsnNode(ALOAD, 1), new MethodInsnNode(INVOKESPECIAL, typeString[CompiledScript], "<init>", s"(${typeSignature[scala.collection.immutable.Map[_, _]]})V"))
    for (g <- globals) {
      val node = new FieldNode(ACC_PRIVATE, s"g_$g", typeSignature[Literal], null, null)
      fields.add(node)
      addIns(constructor, new VarInsnNode(ALOAD, 0), new VarInsnNode(ALOAD, 0), new LdcInsnNode(g), new VarInsnNode(ALOAD, 1),
        new MethodInsnNode(INVOKEVIRTUAL, cn.name, "getGlobal", s"(${typeSignature[String]}${typeSignature[scala.collection.immutable.Map[_, _]]})${typeSignature[Literal]}"),
        new FieldInsnNode(PUTFIELD, cn.name, s"g_$g", typeSignature[Literal])
      )
    }

    addIns(constructor, new InsnNode(RETURN))
    methods.add(constructor)
  }

  private def generateGetGlobals: Unit = {
    val m = new MethodNode(ACC_PUBLIC, "getGlobals", s"()${typeSignature[scala.collection.immutable.Map[String, Literal]]}", null, Array.empty)
    addIns(m, new TypeInsnNode(NEW, typeString[java.util.HashMap[_, _]]), new InsnNode(DUP), new MethodInsnNode(INVOKESPECIAL, typeString[java.util.HashMap[_, _]], "<init>", "()V"), new VarInsnNode(ASTORE, 1))
    for (g <- globals) {
      addIns(m, new VarInsnNode(ALOAD, 1), new LdcInsnNode(g), new VarInsnNode(ALOAD, 0), new FieldInsnNode(GETFIELD, cn.name, s"g_$g", typeSignature[Literal])
        , new MethodInsnNode(INVOKEINTERFACE, typeString[java.util.Map[_, _]], "put", s"(${typeSignature[Object]}${typeSignature[Object]})${typeSignature[Object]}"), new InsnNode(POP))
    }

    addIns(m, new FieldInsnNode(GETSTATIC, typeString[scala.collection.immutable.Map[_, _]] + "$", "MODULE$", s"L${typeString[scala.collection.immutable.Map[_, _]]}$$;")
      , new FieldInsnNode(GETSTATIC, "scala/collection/JavaConversions$", "MODULE$", "Lscala/collection/JavaConversions$;"), new VarInsnNode(ALOAD, 1)
      , new MethodInsnNode(INVOKEVIRTUAL, "scala/collection/JavaConversions$", "asScalaMap", s"(${typeSignature[java.util.Map[_, _]]})${typeSignature[scala.collection.mutable.Map[_, _]]}")
      , new MethodInsnNode(INVOKEINTERFACE, typeString[scala.collection.mutable.Map[_, _]], "toSeq", s"()${typeSignature[scala.collection.Seq[_]]}")
      , new MethodInsnNode(INVOKEVIRTUAL, typeString[scala.collection.immutable.Map[_, _]] + "$", "apply", s"(${typeSignature[scala.collection.Seq[_]]})${typeSignature[scala.collection.GenMap[_, _]]}")
      , new TypeInsnNode(CHECKCAST, typeString[scala.collection.immutable.Map[_, _]])
    )
    addIns(m, new InsnNode(ARETURN))
    methods.add(m)
  }

  private def assignLocalVar(blockInfo: BlockInfo, l: Variable, e: Expression) = {
    val index = getOrDefineLocalVar(blockInfo, l)
    processExpression(blockInfo, e)
    addIns(new VarInsnNode(ASTORE, index))
  }

  private def getOrDefineLocalVar(blockInfo: BlockInfo, l: Variable): Int = blockInfo(l.name).fold(defineLocalVar(blockInfo, l))(x => x)

  private def defineLocalVar(blockInfo: BlockInfo, l: Variable): Int = {
    localVariableCounter += 1
    blockInfo.define(l.name, localVariableCounter)
    localVars.add(new LocalVariableNode(l.name, typeSignature[Literal], typeSignature[Literal], blockInfo.start, blockInfo.end, localVariableCounter))
    localVariableCounter
  }

  private def processElement(blockInfo: BlockInfo, e: ScriptElement): Unit = e match {
    case exp: Expression => processExpression(blockInfo, exp)
    case DeclareVars(l) => l.foreach(x => assignLocalVar(blockInfo, x.l, x.r))
    case assignment: `=` => processAssignment(blockInfo, assignment)
    case `if else`(i, e) => processIfElse(blockInfo, i, e)
    case `while do`(e, b, p) => processWhileDo(blockInfo, e, b, p)
    case `do while`(e, b) => processDoWhile(blockInfo, e, b)
    //    case `switch`(e, c, d) => processSwitch(e, c, d)
    //    case _: `break` => this.break
    //    case _: `continue` => this.breakContinue
    case b: `{}` => processBlock(Some(blockInfo),b)
  }

  private def processIfElse(blockInfo: BlockInfo, _if: `if`, _else: Seq[`else`]) = {
    val blockLabel = new LabelNode()
    var lastLabel = new LabelNode()
    checkCondition(blockInfo, _if.c)

    addIns(new JumpInsnNode(IFEQ, lastLabel))
    processElement(blockInfo, _if.block)
    addIns(new JumpInsnNode(GOTO, blockLabel))
    if (_else.isEmpty) {
      addIns(lastLabel)
    } else for (e <- _else) {
      addIns(lastLabel)
      for (c <- e.c) {
        lastLabel = new LabelNode()
        checkCondition(blockInfo, c)
        addIns(new JumpInsnNode(IFEQ, lastLabel))
      }
      processElement(blockInfo, e.block)
      addIns(new JumpInsnNode(GOTO, blockLabel))
    }

    addIns(blockLabel)

  }

  private def processWhileDo(blockInfo: BlockInfo, condition: Expression, block: `{}`, postFix: Option[ScriptElement]): Unit = {
    val exitLabel = new LabelNode()
    val conditionLabel = new LabelNode()

    addIns(conditionLabel)
    checkCondition(blockInfo, condition)
    addIns(new JumpInsnNode(IFEQ, exitLabel))
    processElement(blockInfo, block)
    postFix.foreach(processElement(blockInfo, _))
    addIns(new JumpInsnNode(GOTO, conditionLabel))
    addIns(exitLabel)
  }

  private def processDoWhile(blockInfo: BlockInfo, condition: Expression, block: `{}`): Unit = {
    val exitLabel = new LabelNode()
    val conditionLabel = new LabelNode()

    addIns(conditionLabel)
    processElement(blockInfo, block)
    checkCondition(blockInfo, condition)
    addIns(new JumpInsnNode(IFEQ, exitLabel), new JumpInsnNode(GOTO, conditionLabel), exitLabel)
  }

  private def getFieldName(l: Literal) = literals.getOrElse(l, createField(l))
  private def checkCondition(blockInfo: BlockInfo, c: Expression): Unit = {
    addIns(new VarInsnNode(ALOAD, 0))
    processExpression(blockInfo, c)
    addIns(new MethodInsnNode(INVOKEVIRTUAL, cn.name, "checkCondition", s"(${typeSignature[Literal]})Z"))
  }
  private def createField(l: Literal) = {
    staticFieldCounter += 1
    val n = s"v$staticFieldCounter"
    val node = new FieldNode(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, s"v$staticFieldCounter", typeSignature[Literal], null, null)
    fields.add(node)
    literals += l -> node.name
    node.name
  }

  private def processExpression(blockInfo: BlockInfo, e: Expression): Unit = {
    e match {
      case LiteralExpression(l: Literal) => fieldGet(l)
      case v: UnaryExpression => processExpression(blockInfo, v.r)
      case v: BinaryExpression => processBinaryExpression(blockInfo, v)
      case GetVar(v: LocalVariable) => processGetLocalVar(blockInfo, v)
      case GetVar(v: GlobalVairable) => processGetGlobalVar(v)
      case `call`(n, a) => processCall(blockInfo, n, a)
      case `++:`(v) => processAssignment(blockInfo, `=`(v, `:+`(GetVar(v), LiteralExpression(DecimalLiteral(BigDecimal(1)))))); processExpression(blockInfo, GetVar(v))
      case `--:`(v) => processAssignment(blockInfo, `=`(v, `:-`(GetVar(v), LiteralExpression(DecimalLiteral(BigDecimal(1)))))); processExpression(blockInfo, GetVar(v))
      case `:++`(v) => processExpression(blockInfo, GetVar(v)); processAssignment(blockInfo, `=`(v, `:+`(GetVar(v), LiteralExpression(DecimalLiteral(BigDecimal(1))))))
      case `:--`(v) => processExpression(blockInfo, GetVar(v)); processAssignment(blockInfo, `=`(v, `:-`(GetVar(v), LiteralExpression(DecimalLiteral(BigDecimal(1))))))
    }
  }

  def processCall(blockInfo: BlockInfo, n: String, a: Seq[Expression]): Unit = {
    addIns(new FieldInsnNode(GETSTATIC, typeString[Functions] + "$", "MODULE$", s"L${typeString[Functions]}$$;"))
    a.reverse.foreach(processExpression(blockInfo, _))
    addIns(new MethodInsnNode(INVOKEVIRTUAL, typeString[Functions] + "$", n, s"(${typeSignature[Literal] * a.length})${if (FunctionInvoker.isReturnLiteral(n)) typeSignature[Literal] else "V"}"))
  }

  private def processGetGlobalVar(v: GlobalVairable): Unit = {
    globals += v.name
    addIns(new VarInsnNode(ALOAD, 0), new FieldInsnNode(GETFIELD, cn.name, s"g_${v.name}", typeSignature[Literal]))
  }

  private def processGetLocalVar(blockInfo: BlockInfo, v: LocalVariable): Unit = {
    if (blockInfo(v.name).isEmpty)
      assignLocalVar(blockInfo, v, LiteralExpression(NullLiteral))
    blockInfo(v.name).foreach(x => addIns(new VarInsnNode(ALOAD,x)))
  }

  private def processBinaryExpression(blockInfo: BlockInfo, v: BinaryExpression): Unit = {
    processExpression(blockInfo, v.l);
    processExpression(blockInfo, v.r);
    invokeOperator(v.getClass.asInstanceOf[Class[_ <: Expression]])
  }

  private def processAssignment(blockInfo: BlockInfo, assignment: `=`) = (assignment.l, assignment.r) match {
    case (v: LocalVariable, e) => assignLocalVar(blockInfo, v, e)
    case (g: GlobalVairable, e) => {
      globals += g.name
      addIns(new VarInsnNode(ALOAD, 0))
      processExpression(blockInfo, e);
      addIns(new FieldInsnNode(PUTFIELD, cn.name, s"g_${g.name}", typeSignature[Literal]))
    }
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
    val script =
      """
        |var i = 10, k = 1, l = 9
        |
        |for(var j = 1; j<10; j++ ){
        |   for(var i = 1;i<10; i+=1){
        |     for(k = 1;k<10;k+=1){
        |        println(str(j) + " " + str(i) + " " + str(k) )
        |     }
        |   }
        |}
        |
        |$result_1 = i++
        |$result_2 = ++i
        |$result_3= l--
        |$result_4 = --l
        |
      """.stripMargin
    println(script)
    val s = EvalScriptParser.load(script)
    val cs = compile(s)

    val instance = cs.getConstructor(classOf[scala.collection.immutable.Map[_, _]]).newInstance(Map.empty[String, Any])
    instance.execute
    println(instance.getGlobals)

  }
}


