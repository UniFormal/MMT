package info.kwarc.mmt.stex.parsing.stex

import info.kwarc.mmt.api.{GlobalName, Level, LocalName, MPath, Path}
import info.kwarc.mmt.lsp.SyncedDocUnparsed
import info.kwarc.mmt.stex.parsing.stex.STeXRules.{AssignRule, DonotCopyRule, RenameDeclRule}
import info.kwarc.mmt.stex.parsing.{Environment, EnvironmentRule, Group, LaTeXParseError, LaTeXParserState, MacroApplication, MacroRule, PlainMacro, PlainText, TeXRule, TeXTokenLike}

case class ModuleRule(dict : DictionaryModule,isimport:Boolean) extends TeXRule {
  val name = "Module " + dict.path
}

trait NonFormalRule extends TeXRule


trait ImportModuleRuleLike extends MacroRule {
  val dict: Dictionary
  val isusemodule: Boolean

  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (optargs, ch) = readOptArg
    children = ch
    val (n, ch2) = readArg
    val a = optargs match {
      case List(List(pt: PlainText)) => pt.str
      case _ => ""
    }
    children = children ::: ch2
    val path = n match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ => {
            plain.addError("Malformed Argument")
            return plain
          }
        }
      case _ =>
        plain.addError("Missing Argument")
        return plain
    }
    val mp = dict.resolveMPath(a, path)
    val ret = ImportModuleApp(plain, mp, children, this, a, path, isusemodule, dict.getModuleOpt)
    safely(ret) {
      dict.addimport(ret)
    }
  }
}

trait SymDeclRuleLike extends MacroRule {
  val dict: Dictionary

  def parseNameAndOpts(file: String)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    var children: List[TeXTokenLike] = Nil
    val makemacro = readChar('*') match {
      case (b, ls) =>
        children = ls.reverse
        !b
    }
    val (nametk, nch) = readArg
    children = children ::: nch
    val maybename = nametk match {
      case g: Group =>
        g.content match {
          case List(pt: PlainText) => pt.str
          case _ => throw LaTeXParseError("Could not determine name for \\symdef", lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Name for \\symdef expected")
    }
    val inmath = state.inmath
    val (opt, nch2) = try {
      state.inmath = true
      readOptArg
    } finally {
      state.inmath = inmath
    }
    children = children ::: nch2
    val (name, args, assoctype, local, err, nopt,defd) = parseSymOpts(opt)
    val gn = dict.getGlobalName(if (name == "") maybename else name)
    (SymdeclInfo(if (makemacro) maybename else gn.toString, gn, args, assoctype, local, file, children.head.startoffset, children.last.endoffset,defd), children, err, nopt)
  }

  def parseSymOpts(ls: List[List[TeXTokenLike]]) = {
    var args = ""
    var ret = ls
    var error = ""
    var name = ""
    var assoctype = ""
    var local = false
    var defd = false
    ls.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.startsWith("args=") =>
          ret = ret.filterNot(_ == l)
          val argstr = s.drop(5)
          if (argstr.forall(_.isDigit)) {
            val arity = argstr.toInt
            args = (0 until arity).map(_ => "i").mkString
          } else {
            argstr.filterNot(List('i', 'a', 'b', 'B').contains) match {
              case "" =>
                args = argstr
              case na =>
                error = "Characters not allowed in args: " + na.mkString(", ")
            }
          }
        case s if s.startsWith("assoc=") =>
          ret = ret.filterNot(_ == l)
          assoctype = s.drop(6)
        case s if s.startsWith("name=") =>
          ret = ret.filterNot(_ == l)
          val rest = s.drop(5)
          name = if (rest.startsWith("{") && rest.endsWith("}")) rest.init.tail else rest
        case s if s.startsWith("gfc=") =>
          ret = ret.filterNot(_ == l)
        case s if s == "local=true" =>
          ret = ret.filterNot(_ == l)
          local = true
        case s if s.startsWith("reorder=") =>
          ret = ret.filterNot(_ == l)
        // TODO?
        case s if s.startsWith("type=") =>
          ret = ret.filterNot(_ == l)
        // TODO?
        case s if s.startsWith("def=") =>
          ret = ret.filterNot(_ == l)
          defd = true
        // TODO?
        case s if s.startsWith("op=") || s.startsWith("prec=") || !s.contains('=') =>
        case _ =>
          print("")
      }
    }
    (name, args, assoctype, local, error, ret,defd)
  }
}


trait NotationRuleLike extends MacroRule {
  val dict: Dictionary

  def optsAndNotation(syminfo: SymdeclInfo, ls: List[List[TeXTokenLike]])(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    val (id, prec, opnot, error, ret) = parseNotOpts(syminfo, ls)
    val (not, nch) = readArg
    var children = nch
    var notation = List(not)
    syminfo.args.filter(c => c == 'a' || c == 'B').foreach { a =>
      val (ret, nch) = readArg
      notation ::= ret
      children = children ::: nch
    }
    (NotationInfo(syminfo, prec, id, notation.reverse, opnot), error, ret, children)
  }

  def parseNotOpts(syminfo: SymdeclInfo, ls: List[List[TeXTokenLike]]) = {
    var ret = ls
    var id = ""
    var error = ""
    var prec: List[Int] = Nil
    var opnot: Option[List[TeXTokenLike]] = None
    ls.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.startsWith("prec=") =>
          ret = ret.filterNot(_ == l)
        //val rest = s.drop(5)
        // TODO
        case s if !s.contains('=') && id == "" =>
          ret = ret.filterNot(_ == l)
          if (s.startsWith("{") && s.endsWith("}")) id = s.init.tail else id = s
        case s if s.startsWith("op=") =>
          ret = ret.filterNot(_ == l)
          opnot = Some(l)
        case _ =>
          print("")
      }
    }
    (id, prec, opnot, error, ret)
  }
}


trait SymRefRuleLike extends MacroRule {
  val dict: Dictionary
  def doAlternatives(ls : List[GlobalName]) = if (ls.length > 1) {
    class mutvar(val path : GlobalName) { var string = path.module.name.toString + "?" + path.name}
    val retstrings = ls.distinct.map(new mutvar(_))
    var done = false
    while (!done) {
      val todos = retstrings.filter(mv => retstrings.exists(o => o.path != mv.path && o.string == mv.string))
      if (todos.isEmpty) done = true
      todos.foreach { td =>
        var idone = false
        var i = 1
        while (!idone) {
          val np = td.path.doc.uri.path.takeRight(i)
          if ((np.mkString("/") + "?" + td.path.module.name + "?" + td.path.name).length <= td.string.length) i += 1 else idone = true
        }
        td.string = td.path.doc.uri.path.takeRight(i).mkString("/") + "?" + td.path.module.name + "?" + td.path.name
      }
    }
    retstrings.map(r => (r.path,r.string))
  } else Nil
  def getNotations(sym : SymdeclInfo)(implicit state: LaTeXParserState) = {
    val allnots = state.rules.flatMap(_.rules).collect {
      case nl:NotationLike if nl.notinfo.syminfo == sym => nl
    }
    allnots.filter(_.isInstanceOf[SetNotationLike]) ::: allnots.filterNot(_.isInstanceOf[SetNotationLike])
  }
  def parseSym(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {
    val (nametk,children) = readArg
    val namestr = nametk match {
      case g : Group =>
        g.content match {
          case List(pt: PlainText) => pt.str
          case _ => throw LaTeXParseError("Could not determine name for \\symdef",lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Name for \\symdef expected")
    }
    val rn = dict.resolveName(namestr)
    rn match {
      case (None,_) =>
        throw LaTeXParseError("Could not find symbol named " + namestr, lvl = Level.Warning)
      case (Some(r),ls) =>
        (r,nametk,doAlternatives(r.path :: ls),children)
    }
  }

}

trait SemanticMacro extends MacroRule with SymRefRuleLike {
  val syminfo:SymdeclInfo
  lazy val name = syminfo.macroname

  def parseInner(plain:PlainMacro,requireNotation:Boolean)
                          (cons : (PlainMacro,List[TeXTokenLike],Option[NotationInfo]) => TeXTokenLike)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) : TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (custom, op) = if (state.inmath) {
      val o = readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      }
      val c = readChar('*') match {
        case (b, ch) =>
          children = children ::: ch.reverse
          b
      }
      (c, o)
    } else {
      (true, readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      })
    }
    if (custom) {
      val (a, ch) = readSafeArg("\\" + plain.name)
      children = children ::: ch
      cons(plain, children, None)
    } else {
      val notations = getNotations(syminfo)
      val notation = readOptArg match {
        case (Nil, ch) =>
          children = children ::: ch
          notations.headOption match {
            case None =>
              val ret = cons(plain, children, None)
              if (requireNotation) ret.addError("No notation found for " + syminfo.path.toString)
              return ret
            case Some(not) =>
              not
          }
        case (ls, ch) =>
          children = children ::: ch
          val notid = ls.map(_.mkString).mkString
          notations.find(_.notinfo.id == notid).getOrElse {
            val ret = cons(plain, children, None)
            if (requireNotation) ret.addError("No notation found for " + syminfo.path.toString)
            return ret
          }
      }
      if (op) {
        val ret = cons(plain, children, Some(notation.notinfo))
        notation.notinfo.opnotation match {
          case None =>
            if (requireNotation) ret.addError("Notation " + notation.notinfo.id + " for " + notation.notinfo.syminfo.path.toString + " has no operator notation!")
          case _ =>
        }
        ret
      } else safely {
        cons(plain, children, Some(notation.notinfo))
      } {
        syminfo.args.foreach { _ =>
          val (_, nch) = readSafeArg("\\" + plain.name)
          children = children ::: nch
        }
        cons(plain, children, Some(notation.notinfo))
      }
    }
  }

  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseInner(plain,true)((pl,ch,nt) => SemanticMacroApp(pl,this,ch,nt))
}

trait VariableRule extends SemanticMacro {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseInner(plain,false)((pl,ch,nt) => SemanticVariableApp(pl,this,ch,nt))
}
trait InstanceRuleLike extends SemanticMacro {
  val module:DictionaryModule

  def parseField(plain: PlainMacro)
                          (cons: (PlainMacro, List[TeXTokenLike], Option[SymdeclInfo], Option[NotationInfo]) => TeXTokenLike)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    var children: List[TeXTokenLike] = Nil
    val (custom, op) = if (state.inmath) {
      val o = readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      }
      val c = if (o) readChar('*') match {
        case (b, ch) =>
          children = children ::: ch.reverse
          b
      } else false
      (c, o)
    } else {
      (true, readChar('!') match {
        case (b, ch) =>
          children = ch.reverse
          b
      })
    }
    if (!custom && op) return cons(plain,children,None,None)
    if (op) {
      val (a, ch) = readSafeArg("\\" + plain.name)
      children = children ::: ch
      return cons(plain, children, None,None)
    }
    val (sym,ch) = readArg
    children = children ::: ch
    val mac = sym match {
      case g: Group =>
        g.content match {
          case List(pt: PlainText) =>
            module.getRules("").collectFirst {
              case mr : SemanticMacro if mr.syminfo.path.name.toString == pt.str => mr
            }.getOrElse {
              throw LaTeXParseError("No field " + pt.str + " found in module " + module.path)
            }
          case _ => throw LaTeXParseError("Could not determine field name for instance \\" + this.name, lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Field name for instance \\" + this.name + " expected")
    }
    mac.parseInner(plain,false){(pl,ch,nt) =>
      cons(pl,children ::: ch,Some(mac.syminfo),nt)
    }
  }
}
trait InstanceFieldRule extends InstanceRuleLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseField(plain)((pl, ch, sd,nt) => StructureFieldApp(pl, this, ch, sd,nt))
}
trait VarInstanceFieldRule extends InstanceRuleLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike =
    parseField(plain)((pl, ch, sd,nt) => VarStructureFieldApp(pl, this, ch, sd,nt))
}

class InlineStatementRule(val name:String,val dict:Dictionary) extends MacroRule {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    val (optargs, ch) = readOptArg
    var name: Option[String] = None
    optargs.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.trim.startsWith("name=") =>
          name = Some(s.drop(5))
        case s if s.trim.startsWith("type=") =>
        case s if s.trim.startsWith("id=") =>
        case s if s.trim.startsWith("for=") =>
        case s if s.trim.startsWith("judgment=") =>
        case _ =>
          throw LaTeXParseError("Unknown key " + s.trim)
      }
    }
    val (_,nch) = readArg
    name match {
      case Some(name) =>
        val gn = dict.getGlobalName(name)
        val self = this
        val ret = new InlineStatement(plain,ch:::nch,this) with SemanticMacro {
          val dict = self.dict
          override val syminfo: SymdeclInfo =
            SymdeclInfo(gn.toString, gn, "", "", false, dict.getFile, plain.startoffset, children.last.endoffset,false)
        }
        val mod = dict.getModule
        mod.exportrules ::= ret
        mod.rules ::= ret
        ret
      case None => new InlineStatement(plain,ch:::nch,this) {}
    }
  }
}
class StatementRule(_name:String,val dict:Dictionary) extends EnvironmentRule(_name) {
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    env.begin match {
      case bs : BeginStatement =>
        bs.name match {
          case None =>
            new Statement(bs, env.end, env.children, this) {}
          case Some(name) =>
            val gn = dict.getGlobalName(name)
            val self = this
            val ret = new Statement(bs,env.end,env.children,this) with SemanticMacro {
              val dict = self.dict
              override val syminfo: SymdeclInfo =
                SymdeclInfo(gn.toString,gn,"","",false,dict.getFile,env.children.head.startoffset,children.last.endoffset,false)
            }
            val mod = dict.getModule
            mod.exportrules ::= ret
            mod.rules ::= ret
            ret
        }
      case _ => env
    }
  }

  protected def doFor(s:String)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {}

  override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val (optargs, ch) = readOptArg
    var name : Option[String] = None
    optargs.foreach { l =>
      val s = l.mkString.flatMap(c => if (c.isWhitespace) "" else c.toString)
      s match {
        case s if s.trim.startsWith("name=") =>
          name = Some(s.drop(5))
        case s if s.trim.startsWith("type=") =>
        case s if s.trim.startsWith("id=") =>
        case s if s.trim.startsWith("title=") =>
        case s if s.trim.startsWith("for=") =>
          s.drop(4).split(',').foreach(st => doFor(st.trim))
        case s if s.trim.startsWith("judgment=") =>
        case _ =>
          throw LaTeXParseError("Unknow key " + s.trim)
      }
    }
    BeginStatement(begin.plain,begin.children ::: ch,this,name)
  }
}

case class MathStructureMacro(
                               pm:PlainMacro,
                               mpi:MPath,
                               macroname:String,
                               symbolpath:GlobalName,
                               ch:List[TeXTokenLike],
                               rl : STeXRules.MathStructureRule,
                               file:String,dict:Dictionary
                             ) extends TeXModuleLike(pm,mpi,ch,rl) with TeXRule with SemanticMacro {
  val sig = ""
  override lazy val name = "mathstructure " + mpi.toString
  val syminfo = SymdeclInfo(macroname,symbolpath,"","",false,this.file,this.startoffset,this.endoffset,true)
}

trait StructureLikeRule extends EnvironmentRule with InStructureRule {
  val dict: Dictionary

  def finalizeStructure(mod:StructureModuleBegin,env:StructureModule)(implicit state: LaTeXParserState) = {}
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    env.begin match {
      case sm: StructureModuleBegin =>
        dict.getModule.macr match {
          case bg: StructureModuleBegin =>
            var allnots = bg.dom.getRules("").collect {
              case nl: NotationLike => nl
            }
            allnots = allnots.filter(_.isInstanceOf[SetNotationLike]) ::: allnots.filterNot(_.isInstanceOf[SetNotationLike])
            bg.domfields.foreach { sm =>
              if (!bg.fields.contains(sm)) {
                val name = getName / sm.syminfo.path.name
                bg.fields(sm) = (Some(name), name.toString, false)
              } else if (bg.fields(sm)._1.isEmpty) {
                val trpl = bg.fields(sm)
                val name = getName / sm.syminfo.path.name
                bg.fields(sm) = (Some(name), trpl._2, trpl._3)
              }
            }
            dict.closeModule
            val self = this
            bg.fields.foreach { case (sm, (Some(newname), macroname, ass)) =>
              val macr = new SemanticMacro {
                val dict = self.dict
                val syminfo = SymdeclInfo(macroname, dict.getGlobalName(newname.toPath), sm.syminfo.args, sm.syminfo.assoctype, false, dict.getFile, bg.startoffset, env.endoffset,ass)
              }
              dict.getModule.exportrules ::= macr
              dict.getModule.rules ::= macr
              allnots.filter(_.notinfo.syminfo == sm.syminfo).foreach { nt =>
                val nr = new MacroApplication(nt.plain, nt.children, this) with NotationLike {
                  override val notinfo: NotationInfo = NotationInfo(
                    macr.syminfo, nt.notinfo.prec, nt.notinfo.id, nt.notinfo.notation, nt.notinfo.opnotation
                  )

                  override def name: String = nt.name
                }
                dict.getModule.exportrules ::= nr
                dict.getModule.rules ::= nr
              }
            }
            val ret = StructureModule(sm, env.end, env.children)
            finalizeStructure(bg,ret)
            ret
          case _ =>
            dict.closeModule
            StructureModule(sm, env.end, env.children)
        }
      case _ => env
    }
  }
  def setupBody[A <:StructureModuleBegin](archive: List[List[TeXTokenLike]], domaintk: TeXTokenLike, nametk: Option[TeXTokenLike])
                                         (make: (MPath,DictionaryModule) => A)(implicit state: LaTeXParserState) = {
    val domstr = domaintk match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ =>
            throw LaTeXParseError("CopyModule missing domain")
        }
      case _ =>
        throw LaTeXParseError("CopyModule missing domain")
    }
    val a = archive match {
      case List(List(pt: PlainText)) => pt.str
      case _ => ""
    }
    val module = dict.requireModule(dict.resolveMPath(a, domstr), a, domstr)

    val name = nametk match {
      case Some(gr: Group) =>
        gr.content match {
          case List(t: PlainText) => LocalName(t.str)
          case _ =>
            throw LaTeXParseError("CopyModule missing name")
        }
      case Some(_) =>
        throw LaTeXParseError("CopyModule missing name")
      case None =>
        LocalName(module.path)
    }

    val mp = dict.getModule.path.parent ? (dict.getModule.path.name/ name)
    val ret = dict.openModule(make(mp,module))
    state.addRule(RenameDeclRule(dict))
    state.addRule(AssignRule(dict))
    state.addRule(DonotCopyRule(dict))
    ret
  }
}

trait InStructureRule extends TeXRule {
  val dict:Dictionary
  def getName = {
    dict.getModule.macr match {
      case bg: StructureModuleBegin =>
        bg.mp.name.last
      case _ =>
        throw LaTeXParseError("Not in CopyModule")
    }
  }
  def addToField(sm: SemanticMacro,name:Option[LocalName] = None,macroname:String = "",isassigned: Boolean = false) = {
    dict.getModule.macr match {
      case bg: StructureModuleBegin =>
        bg.fields.get(sm) match {
          case None =>
            bg.fields(sm) = (name,macroname,isassigned)
          case Some((a,b,c)) =>
            bg.fields(sm) = (
              (a,name) match {
                case (None,Some(n)) => Some(n)
                case (Some(n),None) => Some(n)
                case _ => name
              },
              (b,macroname) match {
                case (s,"") => s
                case _ => macroname
              },
              c || isassigned
            )
        }
      case _ =>
        throw LaTeXParseError("Not in CopyModule")
    }
  }
  def getTarget(tk : TeXTokenLike) = {
    tk match {
      case g: Group =>
        g.content match {
          case List(pt: PlainText) =>
            val tg = pt.str
            dict.getModule.macr match {
              case bg: StructureModuleBegin =>
                bg.domfields.collectFirst {
                  case sm if sm.syminfo.path.name.toString == tg => sm
                }.getOrElse {
                  throw LaTeXParseError("No field " + tg + " found in copymodule")
                }
              case _ =>
                throw LaTeXParseError("Not in CopyModule")
            }
          case _ => throw LaTeXParseError("Could not identify field for \\" + name, lvl = Level.Warning)
        }
      case _ => throw LaTeXParseError("Could not identify field for \\" + name)
    }
  }
}

trait ProofLike extends TeXRule {
  def parseI(plain: PlainMacro,children:List[TeXTokenLike])(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val math = state.inmath
    state.inmath = true
    val (_, ch) = try {
      readOptArg
    } finally {
      state.inmath = math
    }
    val (_, ch2) = readArg
    new MacroApplication(plain, children ::: ch ::: ch2, this)
  }

}
object Proofs {

  lazy val rules = List(ProofEnv("subproof"), ProofMacro("assumption"), ProofMacro("spfstep"),
    ProofMacro("conclude"), new TeXRule with MacroRule {
      val name: String = "eqstep"

      override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
        val math = state.inmath
        state.inmath = true
        val (_, ch) = try {
          readArg
        } finally {
          state.inmath = math
        }
        new MacroApplication(plain, ch, this)
      }
    })
}
case class ProofEnv(_name:String) extends EnvironmentRule(_name) with ProofLike {
  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
  def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val ret = parseI(begin.plain,begin.children)
    Proofs.rules.foreach(state.addRule(_))
    ret
  }
}
case class ProofMacro(name : String) extends MacroRule with ProofLike {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    parseI(plain, Nil)
  }
}

case class MMTInterfaceRule(dict:Dictionary) extends EnvironmentRule("mmtinterface") {
  override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    var children: List[TeXTokenLike] = Nil
    val (pathstr,ch) = readArg
    children = ch
    val mmtpath = pathstr match {
      case gr : Group =>
        gr.content match {
          case List(pt:PlainText) =>
            try { Path.parseM(pt.str)} catch {
              case t =>
                throw LaTeXParseError("Module path expected")
            }
          case _ =>
            throw LaTeXParseError("Missing MMT-URI")
        }
      case _ => throw LaTeXParseError("Missing MMT-URI")
    }
    val (nametk,ch2) = readArg
    children = children ::: ch2
    val name = nametk match {
      case gr: Group =>
        gr.content match {
          case List(t: PlainText) => t.str
          case _ =>
            throw LaTeXParseError("Module name expected")
        }
      case _ =>
        throw LaTeXParseError("Module name expected")
    }
    val stexpath = dict.getMPath(name)
    val ret = MMTInterfaceBegin(begin.plain,mmtpath,stexpath,begin.children ::: children,this)
    dict.openModule(ret)
  }

  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    dict.closeModule
    env
  }
}