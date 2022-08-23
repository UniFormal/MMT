package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api.{GlobalName, SyntaxDrivenRule}

class Compiler {

}

abstract class CompilerEndpoint

abstract case class compiledTerm(compilerEndpoint: CompilerEndpoint, head: GlobalName) extends SyntaxDrivenRule{

}