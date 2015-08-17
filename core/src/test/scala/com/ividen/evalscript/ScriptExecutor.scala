package com.ividen.evalscript

trait ScriptExecutor {
  protected def work(script: Script,globals: Map[String,Any]): Map[String,Any]
  protected final def executeScript(script: String, globals: Map[String,Any] = Map.empty): Map[String,Any] = work(EvalScriptParser.load(script),globals)
}

trait CompiledExecutor extends ScriptExecutor{
  protected def work(script: Script,globals: Map[String,Any]) = ScriptCompiler.execute(ScriptCompiler.compile(script),globals);
}


trait InterpretedExecutor extends ScriptExecutor{
  protected def work(script: Script,globals: Map[String,Any]) = {
    val gc = new GlobalContext(globals)
    Interpreter.execute(script,gc)
    gc.vars
  }
}
