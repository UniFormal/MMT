package info.kwarc.mmt.latex

import info.kwarc.mmt.api._

abstract class StexDeclaration

case class BeginModule(ns: DPath, name: LocalName) extends StexDeclaration {
  override def toString = s"""\begin{module}[id=${name.toPath}]"""
}
case object EndModule extends StexDeclaration {
  override def toString = s"""\end{module}"""
}

case class ImportModule(thy: MPath) extends StexDeclaration {
  override def toString = s"""\importmodule[path=${thy.parent.toPath}]{${thy.name.toPath}}"""
}

case class SymDef(parent: MPath, name: LocalName) extends StexDeclaration {
  override def toString = s"""\symdef{${name.toPath}}"""
}