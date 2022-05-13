package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.History
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Structure}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import info.kwarc.mmt.api.utils.{URI, XMLEscaping}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.rules.{Getfield, ModelsOf, ModuleType, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, ParsingState}

import scala.xml._
import objects._


object OMDocHTML {

  def getNotations(s : GlobalName)(implicit controller:Controller) = {
    val ret = controller.depstore.queryList(s,-NotationExtractor.notation)
    ret.flatMap(controller.getO).collect {
      case c : Constant if c.tp.exists(t => STeX.notation.tp.unapply(t).exists(_._1 == s)) =>
        c.df.flatMap(STeX.notation.unapply).map(t => (c.parent,t._1,t._2,t._3,t._4))
    }.flatten
  }

  def getRuleInfo(tm : Term)(implicit controller:Controller,context:Context) : Option[(List[Int],Option[String],String)] = tm match {
    case OMS(p) => p.name match {
      case LocalName(_ :: ComplexStep(ip) :: n) =>
        getRuleInfo(OMS(ip?n))
      case LocalName(s :: rest) if rest.nonEmpty =>
        controller.getO(p.module ? s) match {
          case Some(s : Structure) => s.tp match {
            case Some(OMPMOD(mod,_)) =>
              getRuleInfo(OMS(mod ? rest))
            case _ => controller.getO(p) match {
              case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
              case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
            }
          }
          case _ => controller.getO(p) match {
            case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
            case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
          }
        }
      case _ => controller.getO(p) match {
        case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
        case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
      }
    }
    case OMV(n) =>
      val vd = context.findLast(_.name == n)
      vd.map(v => (OMDocHTML.getReorder(v),OMDocHTML.getAssoctype(v),OMDocHTML.getArity(v).getOrElse("")))
    case Getfield(t,f) => getOriginal(t,f).flatMap(c => getRuleInfo(c.toTerm))
    case _ => None
  }

  def getOriginal(tm : Term, fieldname : LocalName)(implicit controller:Controller,context:Context) : Option[Constant] = tm match {
    case OMV(n) => context.findLast(_.name == n).flatMap { vd =>
      vd.tp match {
        case Some(ModelsOf(OMPMOD(mp,as))) =>
          ModuleType(mp,as,controller.library).getOrig(fieldname)(controller.library,new History(Nil)) match {
            //Try(controller.library.get(mod,fieldname)).toOption match {
            case Some(c : Constant) => Some(c)
            case _ => None
          }
        case _ => None
      }
    }
    case OMS(p) => controller.getO(p).flatMap {
      case c : Constant =>
        c.tp match {
          case Some(ModelsOf(OMPMOD(mp,as))) =>
            ModuleType(mp,as,controller.library).getOrig(fieldname)(controller.library,new History(Nil)) match {
              //Try(controller.library.get(mod,fieldname)).toOption match {
              case Some(c : Constant) => Some(c)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def setMacroName(se : HasMetaData, s : String) = if (s != "") {
    se.metadata.update(STeX.meta_macro,StringLiterals(s))
  }

  def getMacroName(se : HasMetaData) = se.metadata.getValues(STeX.meta_macro).collectFirst {
    case StringLiterals(mn) => mn
  }

  def setAssoctype(se : HasMetaData, s : String) =  if (s != "") se.metadata.update(STeX.meta_assoctype,StringLiterals(s))
  def getAssoctype(se : HasMetaData) =  se.metadata.getValues(STeX.meta_assoctype).collectFirst {
    case StringLiterals(at) => at
  }
  def setReorder(se : HasMetaData,s : String) = if (s != "") se.metadata.update(STeX.meta_reorder,StringLiterals(s))
  def getReorder(se : HasMetaData) =  se.metadata.getValues(STeX.meta_reorder).collectFirst {
    case StringLiterals(at) => at.split(',').map(_.toInt - 1).toList
  }.getOrElse(Nil)

  def setArity(se : HasMetaData,s : String) = if (s != "") se.metadata.update(STeX.meta_arity,StringLiterals(s))
  def getArity(se : HasMetaData) =  se.metadata.getValues(STeX.meta_arity).collectFirst {
    case StringLiterals(at) => at
  }

  def getNotations(se : StructuralElement,controller : Controller) = {
    val server = controller.extman.get(classOf[STeXServer]).head
    controller.depstore.queryList(se.path,NotationExtractor.notation).map(controller.getO).map {
      case Some(c : Constant) =>
        val arity = c.tp match {
          case Some(STeX.notation.tp(_,a)) => a
          case _ =>
            print("")
            ???
        }
        val (fragment,prec,node,op) = c.df match {
          case Some(STeX.notation(n,p,f,o)) => (f,p,n,o)
          case _ =>
            print("")
            ???
        }
        (fragment,prec,arity,HTMLParser(node.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules))),
          op.map(n => HTMLParser(n.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules)))))
    }
  }
}

object STeX {
  val meta_dpath = DPath(URI.http colon "mathhub.info") / "sTeX" / "meta"
  val meta_path = meta_dpath ? "Metatheory"
  val mmtmeta_path = meta_dpath ? "MMTMeta"

  val string = mmtmeta_path ? "stringliteral"
  val nat = mmtmeta_path ? "natliteral"

  val meta_notation = mmtmeta_path ? "notation"

  val prop = meta_path ? "proposition"

  val notation = new {
    val tp = new {
      val sym = mmtmeta_path ? "notationtype"
      def apply(nsym : GlobalName, arity : String) = OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity)))

      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity))) => Some((nsym,arity))
        case _ => None
      }
    }
    def apply(node : Node, prec : String, frag : String,op:Option[Node]) = {
      op match {
        case Some(op) =>
          OMA(OMS(meta_notation),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node),OMFOREIGN(op)))
        case None =>
          OMA(OMS(meta_notation),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node)))
      }
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`meta_notation`),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node))) =>
        Some((node,prec,frag,None))
      case OMA(OMS(`meta_notation`),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node),OMFOREIGN(op))) =>
        Some((node,prec,frag,Some(op)))
      case _ => None
    }
  }
  val informal = new {
    val sym = mmtmeta_path ? "informalsym"
    val op = new {
      val opsym = mmtmeta_path ? "informalapply"
      def apply(label : String,args : List[Term]) = {
        OMA(OMS(opsym),StringLiterals(label) :: args)
      }
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`opsym`),StringLiterals(label) :: args) => Some((label,args))
        case _ => None
      }
    }
    def apply(n : Node) = OMA(OMS(sym),OMFOREIGN(n) :: Nil)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),OMFOREIGN(n) :: Nil) =>
        Some(n)
      case _ => None
    }
  }

  val flatseq = new {
    val sym = meta_path ? "seqexpr"
    def apply(tms : Term*) = OMA(OMS(sym),tms.toList)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),ls) => Some(ls)
      case _ => None
    }
    val tp = new {
      val sym = meta_path ? "seqtype"
      def apply(tp : Term) = OMA(OMS(sym),List(tp))
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(tp)) => Some(tp)
        case _ => None
      }
    }
  }

  val seqmap = new {
    val sym = meta_path ? "seqmap"
    def apply(seq : Term,v : LocalName,f : Term) = SOMB(OMS(sym),STerm(seq),SCtx(Context(VarDecl(v))),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(seq),SCtx(Context(v)),STerm(f))) =>
        Some((seq,v.name,v.tp,f))
      case _ => None
    }
  }

  val seqprepend = new {
    val sym = meta_path ? "seqprepend"
    def apply(a : Term,seq : Term) = OMA(OMS(`sym`),List(a,seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(a,seq)) =>
        Some((a,seq))
      case _ => None
    }
  }

  val seqappend = new {
    val sym = meta_path ? "seqappend"
    def apply(seq : Term,a : Term) = OMA(OMS(`sym`),List(seq,a))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq,a)) =>
        Some((seq,a))
      case _ => None
    }
  }

  import info.kwarc.mmt.api.objects.Conversions._

  val seqfoldleft = new {
    val sym = meta_path ? "seqfoldleft"
    def apply(init:Term,seq : Term,v1 : LocalName, tp1: Term,v2:LocalName, tp2 : Term,f : Term) = SOMB(OMS(sym),STerm(init),STerm(seq),SCtx(Context(v1 % tp1)),SCtx(v2 % tp2),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) =>
        Some((seq,init,v1.name,v1.tp,v2.name,v2.tp,f))
      case _ => None
    }
  }

  val seqfoldright = new {
    val sym = meta_path ? "seqfoldright"
    def apply(init:Term,seq : Term,v1 : LocalName, tp1: Term,v2:LocalName, tp2 : Term,f : Term) = SOMB(OMS(sym),STerm(init),STerm(seq),SCtx(Context(v1 % tp1)),SCtx(v2 % tp2),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) =>
        Some((seq,init,v1.name,v1.tp,v2.name,v2.tp,f))
      case _ => None
    }
  }

  val seqhead = new {
    val sym = meta_path ? "seqhead"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqtail = new {
    val sym = meta_path ? "seqtail"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqlast = new {
    val sym = meta_path ? "seqlast"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqinit = new {
    val sym = meta_path ? "seqinit"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  import SOMBArg._

  val binder = new {
    val path = meta_path ? "bind"
    def apply(ctx : Context,body : Term) = SOMB(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = SOMB(OMS(path),Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }),body)
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`path`),List(SCtx(Context(vd, rest @_*)),STerm(bd))) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }
  val implicit_binder = new {
    val path = meta_path ? "implicitbind"
    def apply(ctx : Context,body : Term) = SOMB(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = SOMB(OMS(path),Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }),body)
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`path`),List(SCtx(Context(vd, rest @_*)),STerm(bd))) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }

  val symboldoc = new {
    val tp = mmtmeta_path ? "symboldoc"
    val sym = mmtmeta_path ? "symboldocfor"
    def apply(symbol : List[ContentPath],lang : String,doc : HTMLNode) = {
      OMA(OMS(sym),StringLiterals(lang) :: OMFOREIGN(doc.node) :: symbol.map(s => StringLiterals(s.toString))) // OMFOREIGN
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),StringLiterals(lang) :: OMFOREIGN(n) :: ls) =>
        Some((ls.map(StringLiterals.unapply(_).get),lang,n))
      case _ =>
        None
    }
  }

  val judgmentholds = new {
    val sym = meta_path ? "judgmentholds"
    def apply(tm : Term) = OMA(OMS(sym),List(tm))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(t)) => Some(t)
      case _ => None
    }
  }

  val meta_macro = mmtmeta_path ? "macroname"
  val meta_language = mmtmeta_path ? "language"
  val meta_arity = mmtmeta_path ? "arity"
  val meta_assoctype = mmtmeta_path ? "assoctype"
  val meta_reorder = mmtmeta_path ? "reorder"

  val meta_quantification = mmtmeta_path ? "quantification"
  val meta_qforall = mmtmeta_path ? "universal"
  val meta_qexists = mmtmeta_path ? "existential"

  val meta_doctitle = mmtmeta_path ? "doctitle"
}