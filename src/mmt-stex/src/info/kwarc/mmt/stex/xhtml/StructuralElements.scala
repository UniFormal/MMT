package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.STeXExtension
import info.kwarc.mmt.stex.{STeX, SemanticParsingState}

import scala.collection.mutable
import scala.xml.Node


/*



class PreConstant(val path : GlobalName, val parent : PreParent) extends PreElement {
  private var _types : List[Term] = Nil
  private var _notations : List[(String,Node)] = Nil
  def addNotation(frag : String, node : Node) = _notations ::= (frag,node)
  private var _definientia : List[Term] = Nil
  private var _roles : List[String] = Nil
  private var _macronames : List[String] = Nil
  var arity : String = ""
  def addMacro(s : String) = _macronames ::= s
  def addRole(s : String) = _roles ::= s
  def addType(t : Term) = _types ::= t
  def addDefiniens(t : Term) = _definientia ::= t
  override val _parent = Some(parent)
  parent.add(this)
  // TODO multiple types, multiple definitions, roles
  type A = Constant
  def getElementI(implicit state : SemanticParsingState) = {
    val c = Constant(OMID(path.module),path.name,Nil,
      _types.headOption.map(state.applyTerm),
      _definientia.headOption.map(state.applyTerm),
      _roles.headOption,NotationContainer.empty())
    _macronames.headOption.foreach(PreElement.addMacroName(c,_))
    PreElement.addNotations(c,_notations:_*)
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals(arity))
    state.applySE(c)
  }
}

class PreRuleConstant(parent : MPath,val path : ContentPath,args : List[Term],rci : RuleConstantInterpreter) extends PreElement {
  type A = RuleConstant
  def getElementI(implicit state : SemanticParsingState) = {
    rci(parent,OMAorAny(OMID(path),args),true)
  }
}


class PreStructure(val domain : String, val parent : PreTheory, val _name : String = "") extends PreElement with PreParent {
  parent.add(this)
  override val _parent = Some(parent)
  lazy val name = if (_name.nonEmpty) LocalName(_name) else parent.newName(domain)
  var _nonEmpty = false
  def empty = (!_nonEmpty && _children.isEmpty)
  lazy val path = parent.path ? LocalName(name)

  type A = Structure
  // TODO structures, views, ...
  override def getElementI(implicit state : SemanticParsingState) =
    if (empty) state.applySE(PlainInclude(Path.parseM(domain,NamespaceMap.empty),parent.path)) else {
      ???
    }
}

class PreFeature(val path : GlobalName, val feature: String,val parent : PreParent) extends PreParent {
  parent.add(this)
  override val _parent = Some(parent)

  type A = DerivedDeclaration
  // TODO everything
  override def getElementI(implicit state : SemanticParsingState): DerivedDeclaration =
    state.applySE(new DerivedDeclaration(OMID(path.module),LocalName(path.name + "_feature"),feature,TermContainer.empty(),NotationContainer.empty()))
}

class PreTerm(val tm : Term,val parent : PreParent,val path : ContentPath) extends PreElement {
  parent.add(this)
  type A = Constant
  def getElementI(implicit state : SemanticParsingState) = {
    val c = Constant(OMID(path.module),path.name,Nil,None,Some(state.applyTerm(tm)),Some("commentary"))
    state.applySE(c)
  }
}

 */