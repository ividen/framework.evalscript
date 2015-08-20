package com.ividen.evalscript

import java.io.{FileOutputStream, File}
import java.security.MessageDigest
import java.{math, util}

import org.apache.commons.codec.binary.{Hex, Base64, StringUtils}
import org.objectweb.asm.tree._
import org.objectweb.asm.{MethodVisitor, ClassWriter}
import org.objectweb.asm.Opcodes._
import scala.annotation.tailrec
import scala.collection
import scala.collection.{GenMap, mutable}
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable
import scala.reflect.ClassTag


abstract class CompiledScript(globals: collection.immutable.Map[String, Any]) {
  def execute

  def getGlobals: Map[String, Literal]

  protected def checkCondition(l: Literal): Boolean = l.toBooleanLiteral.value == true
  protected def compareLiterals(l: Literal,r: Literal ): Literal = if (checkCondition(l == r)) null else l
  protected def getGlobal(n: String, globals: Map[String, Any]): Literal = Literal.valToLiteral(globals.getOrElse(n, NullLiteral))
}

object Generator {
  private val mapping: Map[Class[_ <: Expression], String] = Map[Class[_ <: Expression], String](
    classOf[`[]`] -> "apply", classOf[`:+`] -> "$plus", classOf[`:-`] -> "$minus", classOf[`/`] -> "$div", classOf[`*`] -> "$times", classOf[`%`] -> "$percent", classOf[`!:`] -> "unary_$bang"
    , classOf[`~:`] -> "unary_$tilde", classOf[`-:`] -> "unary_$minus", classOf[`+:`] -> "unary_$plus", classOf[`<<`] -> "$less$less", classOf[`>>`] -> "$greater$greater", classOf[`&`] -> "$amp"
    , classOf[`^`] -> "$up", classOf[`|`] -> "$bar", classOf[`&&`] -> "$amp$amp", classOf[`||`] -> "$bar$bar", classOf[`:==`] -> "$eq$eq", classOf[`:!=`] -> "$bang$eq", classOf[`<`] -> "$less"
    , classOf[`>`] -> "$greater", classOf[`>=`] -> "$greater$eq", classOf[`<=`] -> "$less$eq")

  val SIGN_MATH_BIG_DECIMAL: String = typeSignature[math.BigDecimal]
  val SIGN_BIG_DECIMAL: String = typeSignature[BigDecimal]
  val SIGN_LITERAL: String = typeSignature[Literal]
  val SIGN_STRING: String = typeSignature[String]
  val SIGN_UTIL_LIST: String = typeSignature[util.List[_]]
  val SIGN_BUFFER: String = typeSignature[collection.mutable.Buffer[_]]
  val SIGN_VECTOR: String = typeSignature[Vector[_]]
  val SIGN_ARRAY_LITERAL: String = typeSignature[ArrayLiteral]
  val SIGN_UTIL_MAP: String = typeSignature[util.Map[_, _]]
  val SIGN_MUTABLE_MAP: String = typeSignature[collection.mutable.Map[_, _]]
  val SIGN_SEQ: String = typeSignature[Seq[_]]
  val SIGN_GEN_MAP: String = typeSignature[GenMap[_, _]]
  val SIGN_IMMUTABLE_MAP: String = typeSignature[Map[_, _]]
  val TYPE_MUTABLE_MAP: String = typeString[collection.mutable.Map[_, _]]
  val TYPE_BOOLEAN_LITERAL: String = typeString[BooleanLiteral]
  val TYPE_STRING_LITERAL: String = typeString[StringLiteral]
  val TYPE_COMPILED_SCRIPT: String = typeString[CompiledScript]
  val TYPE_UTIL_ARRAYS: String = typeString[util.Arrays]
  val TYPE_MATH_BIG_DECIMAL: String = typeString[math.BigDecimal]
  val TYPE_FUNCTIONS: String = typeString[Functions]
  val TYPE_BIG_DECIMAL: String = typeString[BigDecimal]
  val TYPE_DECIMAL_LITERAL: String = typeString[DecimalLiteral]
  val TYPE_ARRAY_LITERAL: String = typeString[ArrayLiteral]
  val TYPE_BUFFER: String = typeString[collection.mutable.Buffer[_]]
  val TYPE_LITERAL: String = typeString[Literal]
  val TYPE_UTIL_HASH_MAP: String = typeString[util.HashMap[_, _]]
  val TYPE_IMMUTABLE_MAP: String = typeString[Map[_, _]]

  private def typeSignature[T](implicit tag: ClassTag[T]) = "L" + typeString[T](tag) + ";"
  private def typeString[T](implicit tag: ClassTag[T]) = tag.runtimeClass.getName.replaceAll("\\.", "/")
}

private class Generator(b: `{}`, name: String) extends ClassLoader(Thread.currentThread().getContextClassLoader) {

  import Generator._

  case class BlockInfo(start: LabelNode, end: LabelNode, prevBlock: Option[BlockInfo]) {
    private val localVarsMapping: collection.mutable.Map[String, Int] = new collection.mutable.HashMap[String, Int]
    var currentLoopMarker : Option[LabelNode Pair LabelNode] = None

    def apply(n: String): Option[Int] = localVarsMapping.get(n).fold(prevBlock.flatMap(b => b.apply(n)))(Some(_))
    def define(n: String, index: Int): Unit = localVarsMapping += (n -> index)
    def loopMarker:  Option[LabelNode Pair LabelNode] =  currentLoopMarker.fold(prevBlock.flatMap(b => b.loopMarker))(Some(_))
  }

  private val cn = new ClassNode(ASM4)
  private val exec = new MethodNode(ACC_PUBLIC, "execute", "()V", null, Array.empty)
  private val methods: java.util.List[MethodNode] = cn.methods.asInstanceOf[java.util.List[MethodNode]]
  private val fields: java.util.List[FieldNode] = cn.fields.asInstanceOf[java.util.List[FieldNode]]
  private val literals: collection.mutable.Map[Literal, String] = new collection.mutable.LinkedHashMap[Literal, String]()
  private val globals: collection.mutable.Set[String] = new collection.mutable.HashSet[String]()
  private val localVars: java.util.List[LocalVariableNode] = exec.localVariables.asInstanceOf[java.util.List[LocalVariableNode]]

  private var staticFieldCounter: Int = 0
  private var localVariableCounter: Int = 0

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
    cw.toByteArray
  }

  private def initStaticVars = {
    if (!literals.isEmpty) {
      preProcessLiterals
      val m = new MethodNode(ACC_STATIC, "<clinit>", "()V", null, Array.empty)
      for (e <- literals.clone()) {
        initLiteral(m, e)
      }
      m.instructions.add(new InsnNode(RETURN))
      methods.add(m)
    }
  }

  private def preProcessLiterals(): Unit ={
    val result: collection.mutable.Map[Literal, String] = new collection.mutable.LinkedHashMap

    def preProcess(x: (Literal,String)): Unit =  x._1 match   {
      case l: ArrayLiteral => {
        for(e <- l.value if !result.contains(e)) {
          preProcess(e,createField(l,result))
        }
        result += x
      }
      case _ => result += x
    }

    literals.foreach(preProcess)
    literals.clear()
    literals ++= result
  }

  private def processBlock(parentBlock: Option[BlockInfo], b: `{}`) = {
    val blockInfo = createBlockInfo(parentBlock)
    addIns(blockInfo.start)
    b.items.foreach(processElement(blockInfo, _))
    addIns(blockInfo.end)
  }

  private def createBlockInfo(parentBlock: Option[BlockInfo]): BlockInfo = BlockInfo(new LabelNode(), new LabelNode(),  parentBlock)
  private def addIns(il: AbstractInsnNode*) = il.foreach(exec.instructions.add)
  private def addIns(m: MethodNode, il: AbstractInsnNode*) = il.foreach(m.instructions.add)

  private def initLiteral(m: MethodNode, e: (Literal, String)): Unit = {
    e._1 match {
      case DecimalLiteral(v) => initDecimal(m, e._2, v)
      case StringLiteral(v) => initString(m, e._2, v)
      case BooleanLiteral(v) => initBoolean(m, e._2, v)
      case ArrayLiteral(v) => initArray(m,e._2,v)
      case _ =>
    }
  }

  private def initDecimal(m: MethodNode, n: String, v: BigDecimal): Unit = addIns(m, new TypeInsnNode(NEW, TYPE_DECIMAL_LITERAL), new InsnNode(DUP)
    , new TypeInsnNode(NEW, TYPE_BIG_DECIMAL), new InsnNode(DUP)
    , new TypeInsnNode(NEW, TYPE_MATH_BIG_DECIMAL), new InsnNode(DUP)
    , new LdcInsnNode(v.toString()), new MethodInsnNode(INVOKESPECIAL, TYPE_MATH_BIG_DECIMAL, "<init>", s"(${SIGN_STRING})V")
    , new MethodInsnNode(INVOKESPECIAL, TYPE_BIG_DECIMAL, "<init>", s"(${SIGN_MATH_BIG_DECIMAL})V")
    , new MethodInsnNode(INVOKESPECIAL, TYPE_DECIMAL_LITERAL, "<init>", s"(${SIGN_BIG_DECIMAL})V"), new FieldInsnNode(PUTSTATIC, cn.name, n, SIGN_LITERAL))

  private def initString(m: MethodNode, n: String, v: String): Unit = addIns(m, new TypeInsnNode(NEW, TYPE_STRING_LITERAL), new InsnNode(DUP)
    , new LdcInsnNode(v), new MethodInsnNode(INVOKESPECIAL, TYPE_STRING_LITERAL, "<init>", s"(${SIGN_STRING})V")
    , new FieldInsnNode(PUTSTATIC, cn.name, n, SIGN_LITERAL))

  private def initBoolean(m: MethodNode, n: String, v: Boolean) = addIns(m, new TypeInsnNode(NEW, TYPE_BOOLEAN_LITERAL)
    , new InsnNode(DUP)
    , new LdcInsnNode(v)
    , new MethodInsnNode(INVOKESPECIAL, TYPE_BOOLEAN_LITERAL, "<init>", s"(Z)V")
    , new FieldInsnNode(PUTSTATIC, cn.name, n, SIGN_LITERAL))
  private def initArray(m: MethodNode, n: String, v: Vector[Literal]) = {
    addIns(m, new FieldInsnNode(GETSTATIC, s"${TYPE_ARRAY_LITERAL}$$", "MODULE$", s"L${TYPE_ARRAY_LITERAL}$$;")
      , new FieldInsnNode(GETSTATIC, "scala/collection/JavaConversions$", "MODULE$", "Lscala/collection/JavaConversions$;"), new IntInsnNode(BIPUSH, v.size), new TypeInsnNode(ANEWARRAY, TYPE_LITERAL))
    for (i <- 0 to v.size-1) {
      addIns(m, new InsnNode(DUP), new IntInsnNode(BIPUSH, i)  ,new FieldInsnNode(GETSTATIC, cn.name, getFieldName(v(i)), SIGN_LITERAL)  , new InsnNode(AASTORE))
    }

    addIns(m, new MethodInsnNode(INVOKESTATIC, TYPE_UTIL_ARRAYS, "asList", s"([Ljava/lang/Object;)${SIGN_UTIL_LIST}")
      , new MethodInsnNode(INVOKEVIRTUAL, "scala/collection/JavaConversions$", "asScalaBuffer", s"(${SIGN_UTIL_LIST})${SIGN_BUFFER}")
      , new MethodInsnNode(INVOKEINTERFACE, TYPE_BUFFER, "toVector", s"()${SIGN_VECTOR}")
      , new MethodInsnNode(INVOKEVIRTUAL, s"${TYPE_ARRAY_LITERAL}$$", "apply", s"(${SIGN_VECTOR})${SIGN_ARRAY_LITERAL}")
      , new FieldInsnNode(PUTSTATIC, cn.name, n, SIGN_LITERAL))
  }

  private def initClassName: Unit = cn.visit(V1_5, ACC_PUBLIC, name, null, TYPE_COMPILED_SCRIPT, Array.empty)
  private def generateConstructor: Unit = {
    val constructor = new MethodNode(ACC_PUBLIC, "<init>", s"(${SIGN_IMMUTABLE_MAP})V", null, Array.empty)
    addIns(constructor, new VarInsnNode(ALOAD, 0), new VarInsnNode(ALOAD, 1), new MethodInsnNode(INVOKESPECIAL, TYPE_COMPILED_SCRIPT, "<init>", s"(${SIGN_IMMUTABLE_MAP})V"))
    for (g <- globals) {
      val node = new FieldNode(ACC_PRIVATE, s"g_$g", SIGN_LITERAL, null, null)
      fields.add(node)
      addIns(constructor, new VarInsnNode(ALOAD, 0), new VarInsnNode(ALOAD, 0), new LdcInsnNode(g), new VarInsnNode(ALOAD, 1),
        new MethodInsnNode(INVOKEVIRTUAL, cn.name, "getGlobal", s"(${SIGN_STRING}${SIGN_IMMUTABLE_MAP})${SIGN_LITERAL}"),
        new FieldInsnNode(PUTFIELD, cn.name, s"g_$g", SIGN_LITERAL)
      )
    }

    addIns(constructor, new InsnNode(RETURN))
    methods.add(constructor)
  }

  private def generateGetGlobals: Unit = {
    val m = new MethodNode(ACC_PUBLIC, "getGlobals", s"()${SIGN_IMMUTABLE_MAP}", null, Array.empty)
    addIns(m, new TypeInsnNode(NEW, TYPE_UTIL_HASH_MAP), new InsnNode(DUP)
      , new MethodInsnNode(INVOKESPECIAL, TYPE_UTIL_HASH_MAP, "<init>", "()V"), new VarInsnNode(ASTORE, 1))
    for (g <- globals) {
      addIns(m , new FieldInsnNode(GETSTATIC, s"${TYPE_LITERAL}$$", "MODULE$", s"L${TYPE_LITERAL}$$;") , new VarInsnNode(ALOAD, 0), new FieldInsnNode(GETFIELD, cn.name, s"g_$g", SIGN_LITERAL)
        , new LdcInsnNode(g), new VarInsnNode(ALOAD, 1) , new MethodInsnNode(INVOKEVIRTUAL, s"${TYPE_LITERAL}$$",  "resultToMap", s"(${SIGN_LITERAL}${SIGN_STRING}${SIGN_UTIL_MAP})V"))
    }

    addIns(m, new FieldInsnNode(GETSTATIC, TYPE_IMMUTABLE_MAP + "$", "MODULE$", s"L${TYPE_IMMUTABLE_MAP}$$;")
      , new FieldInsnNode(GETSTATIC, "scala/collection/JavaConversions$", "MODULE$", "Lscala/collection/JavaConversions$;"), new VarInsnNode(ALOAD, 1)
      , new MethodInsnNode(INVOKEVIRTUAL, "scala/collection/JavaConversions$", "asScalaMap", s"(${SIGN_UTIL_MAP})${SIGN_MUTABLE_MAP}")
      , new MethodInsnNode(INVOKEINTERFACE, TYPE_MUTABLE_MAP, "toSeq", s"()${SIGN_SEQ}")
      , new MethodInsnNode(INVOKEVIRTUAL, TYPE_IMMUTABLE_MAP + "$", "apply", s"(${SIGN_SEQ})${SIGN_GEN_MAP}")
      , new TypeInsnNode(CHECKCAST, TYPE_IMMUTABLE_MAP)
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
    localVars.add(new LocalVariableNode(l.name, SIGN_LITERAL, SIGN_LITERAL, blockInfo.start, blockInfo.end, localVariableCounter))
    localVariableCounter
  }

  private def processElement(blockInfo: BlockInfo, e: ScriptElement): Unit = (e: @unchecked) match {
    case exp: Expression => processExpression(blockInfo, exp)
    case DeclareVars(l) => l.foreach(x => assignLocalVar(blockInfo, x.l, x.r))
    case assignment: `=` => processAssignment(blockInfo, assignment)
    case `if else`(i, e) => processIfElse(blockInfo, i, e)
    case `while do`(e, b, p) => processWhileDo(blockInfo, e, b, p)
    case `do while`(e, b) => processDoWhile(blockInfo, e, b)
    case `switch`(e, c, d) => processSwitch(blockInfo,e, c, d)
    case _: `break` => processBreak(blockInfo)
    case _: `continue` => processContinue(blockInfo)
    case b: `{}` => processBlock(Some(blockInfo), b)
  }

  private def processContinue(blockInfo: BlockInfo): Unit = blockInfo.loopMarker.foreach(x => addIns(new JumpInsnNode(GOTO, x._1)))
  private def processBreak(blockInfo: BlockInfo): Unit =   blockInfo.loopMarker.foreach(x => addIns(new JumpInsnNode(GOTO, x._2)))
  private def processIfElse(blockInfo: BlockInfo, _if: `if`, _else: Seq[`else`]) = {
    val blockLabel: LabelNode = processIff(blockInfo, _if)
    _else.foreach(processElse(blockInfo, blockLabel, _))
    addIns(blockLabel)
  }

  private def processElse(blockInfo: BlockInfo, blockLabel: LabelNode, e: `else`): Unit = {
    val lastLabel = new LabelNode()
    for (c <- e.c) {
      checkCondition(blockInfo, c)
      addIns(new JumpInsnNode(IFEQ, lastLabel))
    }
    processElement(blockInfo, e.block)
    addIns(new JumpInsnNode(GOTO, blockLabel))
    addIns(lastLabel)
  }

  private def processIff(blockInfo: BlockInfo, _if: `if`): LabelNode = {
    val blockLabel = new LabelNode()
    val lastLabel = new LabelNode()
    checkCondition(blockInfo, _if.c)

    addIns(new JumpInsnNode(IFEQ, lastLabel))
    processElement(blockInfo, _if.block)
    addIns(new JumpInsnNode(GOTO, blockLabel),lastLabel)
    blockLabel
  }

  private def processSwitch(blockInfo: BlockInfo, e: Expression, cases: Seq[`case`], default: Option[`{}`]) = {
    val lastLabel = new LabelNode()
    val defaultLabel = new LabelNode()
    val labels: Array[LabelNode] = Array.fill[LabelNode](cases.size)(new LabelNode())

    addIns(new VarInsnNode(ALOAD, 0))
    processExpression(blockInfo,e)

    val indexedCases = cases.zipWithIndex

    for((c,index) <- indexedCases){
      processExpression(blockInfo,c.e)
      compareLiterals(blockInfo)
      addIns(new InsnNode(DUP),new JumpInsnNode(IFNULL,labels(index)),new VarInsnNode(ALOAD, 0), new InsnNode(SWAP))
    }

    addIns(new InsnNode(POP),new JumpInsnNode(GOTO,defaultLabel))

    for((c,index) <- indexedCases){
      val blockLabel = labels(index)
      val prevMarker = blockInfo.currentLoopMarker
      blockInfo.currentLoopMarker = Some(blockLabel,lastLabel)
      addIns(blockLabel)
      c.b.foreach(processElement(blockInfo,_))
      blockInfo.currentLoopMarker = prevMarker
    }

    addIns(defaultLabel)
    default.foreach(x => {processElement(blockInfo, x); addIns (new JumpInsnNode(GOTO, lastLabel))})

    addIns(lastLabel)
  }

  private def processWhileDo(blockInfo: BlockInfo, condition: Expression, block: `{}`, postFix: Option[ScriptElement]): Unit = {
    val exitLabel = new LabelNode()
    val conditionLabel = new LabelNode()
    val postFixLabel = new LabelNode()

    addIns(conditionLabel)
    checkCondition(blockInfo, condition)
    addIns(new JumpInsnNode(IFEQ, exitLabel))
    processLoop(blockInfo,(postFixLabel,exitLabel),block)
    addIns(postFixLabel)
    postFix.foreach(processElement(blockInfo, _))
    addIns(new JumpInsnNode(GOTO, conditionLabel))
    addIns(exitLabel)
  }

  private def processDoWhile(blockInfo: BlockInfo, condition: Expression, block: `{}`): Unit = {
    val exitLabel = new LabelNode()
    val conditionLabel = new LabelNode()

    addIns(conditionLabel)
    processLoop(blockInfo,(conditionLabel,exitLabel),block)
    checkCondition(blockInfo, condition)
    addIns(new JumpInsnNode(IFEQ, exitLabel), new JumpInsnNode(GOTO, conditionLabel), exitLabel)
  }

  private def processLoop(blockInfo: BlockInfo,labels: (LabelNode,LabelNode), block: `{}`) = {
    val prev = blockInfo.currentLoopMarker
    blockInfo.currentLoopMarker = Some(labels)
    processElement(blockInfo, block)
    blockInfo.currentLoopMarker = prev
  }

  private def getFieldName(l: Literal) = literals.getOrElse(l, createField(l))
  private def checkCondition(blockInfo: BlockInfo, c: Expression): Unit = {
    addIns(new VarInsnNode(ALOAD, 0))
    processExpression(blockInfo, c)
    addIns(new MethodInsnNode(INVOKEVIRTUAL, cn.name, "checkCondition", s"(${SIGN_LITERAL})Z"))
  }

  private def compareLiterals(blockInfo: BlockInfo): Unit = addIns(new MethodInsnNode(INVOKEVIRTUAL, cn.name, "compareLiterals", s"(${SIGN_LITERAL}${SIGN_LITERAL})${SIGN_LITERAL}"))

  private def createField(l: Literal, map: collection.mutable.Map[Literal,String] = literals) = {
    staticFieldCounter += 1
    val n = s"v$staticFieldCounter"
    val node = new FieldNode(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, s"v$staticFieldCounter", SIGN_LITERAL, null, null)
    fields.add(node)
    literals += l -> node.name
    node.name
  }

  private def processExpression(blockInfo: BlockInfo, e: Expression): Unit = {
    e match {
      case LiteralExpression(l: Literal) => fieldGet(l)
      case v: UnaryExpression =>     processUnaryExpression(blockInfo, v)
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

  private def processCall(blockInfo: BlockInfo, n: String, a: Seq[Expression]): Unit = {
    addIns(new FieldInsnNode(GETSTATIC, TYPE_FUNCTIONS + "$", "MODULE$", s"L${TYPE_FUNCTIONS}$$;"))
    a.reverse.foreach(processExpression(blockInfo, _))
    addIns(new MethodInsnNode(INVOKEVIRTUAL, TYPE_FUNCTIONS + "$", n, s"(${SIGN_LITERAL * a.length})${if (FunctionInvoker.isReturnLiteral(n)) SIGN_LITERAL else "V"}"))
  }

  private def processGetGlobalVar(v: GlobalVairable): Unit = {
    globals += v.name
    addIns(new VarInsnNode(ALOAD, 0), new FieldInsnNode(GETFIELD, cn.name, s"g_${v.name}", SIGN_LITERAL))
  }

  private def processGetLocalVar(blockInfo: BlockInfo, v: LocalVariable): Unit = {
    if (blockInfo(v.name).isEmpty)
      assignLocalVar(blockInfo, v, LiteralExpression(NullLiteral))
    blockInfo(v.name).foreach(x => addIns(new VarInsnNode(ALOAD, x)))
  }

  private def processBinaryExpression(blockInfo: BlockInfo, v: BinaryExpression): Unit = {
    processExpression(blockInfo, v.l);
    processExpression(blockInfo, v.r);
    invokeBinaryOperator(v.getClass.asInstanceOf[Class[_ <: Expression]])
  }

  private def processUnaryExpression(blockInfo: BlockInfo, v: UnaryExpression): Unit = {
    processExpression(blockInfo, v.r)
    invokeUnaryOperator(v.getClass.asInstanceOf[Class[_ <: Expression]])
  }

  private def processAssignment(blockInfo: BlockInfo, assignment: `=`) = (assignment.l, assignment.r) match {
    case (v: LocalVariable, e) => assignLocalVar(blockInfo, v, e)
    case (g: GlobalVairable, e) => {
      globals += g.name
      addIns(new VarInsnNode(ALOAD, 0))
      processExpression(blockInfo, e);
      addIns(new FieldInsnNode(PUTFIELD, cn.name, s"g_${g.name}", SIGN_LITERAL))
    }
  }

  private def invokeBinaryOperator(c: Class[_ <: Expression]) = addIns(new MethodInsnNode(INVOKEINTERFACE, TYPE_LITERAL, mapping.getOrElse(c, ""), s"(${SIGN_LITERAL})${SIGN_LITERAL}"))
  private def invokeUnaryOperator(c: Class[_ <: Expression]) = addIns(new MethodInsnNode(INVOKEINTERFACE, TYPE_LITERAL, mapping.getOrElse(c, ""), s"()${SIGN_LITERAL}"))
  private def fieldGet(l: Literal) = addIns(new FieldInsnNode(GETSTATIC, cn.name, getFieldName(l), SIGN_LITERAL))
}

object ScriptCompiler {
  def compile(s: Script,name: Option[String] = None) = new Generator(s.block,name.getOrElse(generateClassName(s))).compile
  def execute[T<:CompiledScript](c: Class[T], globals: Map[String,Any]): Map[String,Any] = {
    val instance = c.getConstructor(classOf[Map[_, _]]).newInstance(globals)
    instance.execute
    instance.getGlobals
  }

  private def generateClassName(s: Script): String =   s"G${Hex.encodeHexString(MessageDigest.getInstance("MD5").digest(s.toString.getBytes))}"
}


