package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.{DeclaredLink, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.symbols.{DeclaredStructure, FinalConstant}

import scala.util.{Success, Try}

/**
  * Top class for all translations
  */
abstract class Translation {
  val source : List[FullArchive]
  val target : List[FullArchive]

  // returns Some((p,l)) if applicable (None if not), where p is the symbol it applies to
  // (needed to prevent circular translation) and l the list of newly introduced symbols
  def isApplicable(t : Term) : Option[(GlobalName,List[GlobalName])]

  protected def translate(t : Term) : Term
  def apply(t : Term) : Term = {
    if (isApplicable(t).isDefined) translate(t) else t
  }

  override def toString = "Translation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ")
}

/**
  * Groups translations together. Translations in the same group are preferably used jointly
  */
abstract class TranslationGroup {
  val translations : List[Translation]
  val source : List[FullArchive]
  val target : List[FullArchive]

  // similar as above, but in addition returns the specific translations that are applicable
  def isApplicable(t : Term) : List[(GlobalName,List[GlobalName],Translation)] = translations.collect{
    case tr if tr.isApplicable(t).isDefined =>
      val ret = tr.isApplicable(t).get
      (ret._1,ret._2,tr)
  }

  override def toString = "TranslationGroup from " + source.mkString(", ") + " to " + target.mkString(", ") +
    translations.map(" - " + _.toString).mkString("\n")
}

abstract class SimpleTranslation extends Translation {
  val from : GlobalName
  val to : List[GlobalName]
}

sealed abstract class AlignmentTranslation(alignment : FormalAlignment) extends SimpleTranslation {
  require(alignment.to.mmturi.isInstanceOf[GlobalName])
  require(alignment.from.mmturi.isInstanceOf[GlobalName])
  val from = alignment.to.mmturi.asInstanceOf[GlobalName]
  val to = List(alignment.to.mmturi.asInstanceOf[GlobalName])

  override def toString = "AlignmentTranslation from " + source.map(_.name).mkString(", ") + " to " +
    target.map(_.name).mkString(", ") + "\nusing " + alignment.toString
}

case class SimpleLink(from : GlobalName, toTerm : Term, source : List[FullArchive], target: List[FullArchive], link:DeclaredLink) extends SimpleTranslation {
  val to = ArchiveStore.getSymbols(toTerm)

  lazy val toSymbols = ArchiveStore.getSymbols(toTerm)

  def isApplicable(t : Term) = t match {
    case OMS(p) if p == from => Some((p,toSymbols))
    case _ => None
  }

  protected def translate(t : Term) : Term = toTerm

  override def toString = "LinkTranslation from " + from.toString + " to " + toTerm.toString
}

sealed abstract class LinkTranslation extends TranslationGroup {
  val link : DeclaredLink
}

case class InverseViewTranslation(link : DeclaredView, source : List[FullArchive], target : List[FullArchive]) extends LinkTranslation {
  lazy val translations : List[Translation] = link.getDeclarations.collect{
    case x : FinalConstant if x.df.isDefined =>
      (x.name,x.df.get) match {
        case (LocalName(ComplexStep(path : MPath) :: ln),OMS(p)) =>
          Some(SimpleLink(p,OMS(path ? ln),source,target,link))
        case _ => None
      }
  }.collect{
    case Some(t) => t
  }

  override def toString = "InverseViewTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
    "\nusing " + link.path.toString + "\n" +
    translations.map(" - " + _.toString).mkString("\n")
}

case class ViewTranslation(link : DeclaredView, source : List[FullArchive], target : List[FullArchive]) extends LinkTranslation {
  lazy val translations = link.getDeclarations.collect{
    case x : FinalConstant if x.df.isDefined =>
      x.name match {
        case LocalName(ComplexStep(path : MPath) :: ln) =>
          Some(SimpleLink(path ? ln, x.df.get,source,target,link))
        case _ => None
      }
  }.collect{
    case Some(t) => t
  }

  override def toString = "ViewTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
    "\nusing " + link.path.toString + "\n" +
    translations.map(" - " + _.toString).mkString("\n")
}



class Translator extends Extension {
  override def logPrefix = "Translator"

  override def start(args: List[String]) = {

  }

  private case class SimpleAlignmentTranslation(a : SimpleAlignment, source : List[FullArchive], target : List[FullArchive])
    extends AlignmentTranslation(a) {
    def isApplicable(t : Term) = t match {
      case OMS(p) if p == from => Some((from,to))
      case _ => None
    }
    protected def translate(t : Term) : Term = t match {
      case OMS(p) if p == from => OMS(to.head)
      case _ => t // can not ever occur
    }
  }

  private case class ArgumentAlignmentTranslation(align : ArgumentAlignment, source : List[FullArchive], target : List[FullArchive])
    extends AlignmentTranslation(align) {

    abstract class Application {
      val symbol : Option[GlobalName]
      def unapply(t : Term) : Option[(Term,List[Term])]
      def apply(f : Term, args : List[Term], ls : List[GlobalName]) : Term
    }

    object OMApplication extends Application {
      val symbol = None
      def apply(f : Term, args : List[Term], ls : List[GlobalName]) : Term = if (ls.isEmpty) OMA(f,args) else {
        val app = SymbolApplication(ls.head,this)
        app(f,args,ls.tail)
      }
      def unapply(t : Term) = t match {
        case OMA(f,args) => Some((f,args))
        case _ => None
      }
    }
    case class SymbolApplication(s : GlobalName, app : Application) extends Application {
      val symbol = Some(s)
      def apply(f : Term, args : List[Term], ls : List[GlobalName]) : Term = if (ls.isEmpty) args.foldLeft(f)((o,a) => app(OMS(s),List(o,a),Nil))
      else {
        val napp = SymbolApplication(ls.head,this)
        napp(f,args,ls.tail)
      }
      def unapply(t : Term) : Option[(Term,List[Term])] = t match {
        case app(OMS(sx), f :: args) if sx == s =>
          unapply(f) match {
            case None => Some((f, args))
            case Some((c, args0)) => Some((c, args0 ::: args))
          }
        case _ => None
      }
    }

    object Application {
      def apply(f : Term, args : List[Term], ls : List[GlobalName]) = OMApplication(f,args,ls)
      def unapply(t : Term) : Option[(Term,List[Term],List[GlobalName])] = smartunapply(t,OMApplication)
      private def smartunapply(t : Term, app : Application) : Option[(Term,List[Term],List[GlobalName])] = t match {
        case app(OMS(f),args) =>
          controller.get(f) match {
            case c : FinalConstant if c.rl contains "apply" =>
              val napp = SymbolApplication(f,app)
              smartunapply(t,napp).map(r => (r._1,r._2,f :: r._3))
            case _ =>
              Some((OMS(f),args,Nil))
          }
        case app(f,args) =>
          Some((f,args,Nil))
        case _ => None
      }
    }

    def isApplicable(t : Term) = t match {
      case OMS(p) if p == from => Some((from,to))
      case Application(OMS(p),args,_) if p == from => Some((from,to))
      case _ => None
    }
    private def reorder(ts : List[Term]) : List[Term] = {
      val max = align.arguments.maxBy(p => p._2)._2
      (1 to max).map(i => {
        val ni = align.arguments.find(p => p._2 == i).map(_._1)
        if (ni.isEmpty) OMV(LocalName("_")) // TODO implicit arguments
        else ts(ni.get)
      }).toList
    }
    protected def translate(t : Term) : Term = t match {
      case OMS(p) if p == from => OMS(to.head)
      case Application(OMS(p),args,appls) if p == from => Application(OMS(to.head),reorder(args),appls)
      case _ => t // can not ever occur
    }
  }

  case class StructureTranslation(link : DeclaredStructure, source : List[FullArchive], target : List[FullArchive]) extends LinkTranslation {
    val translations = link.from match {
      case OMMOD(p) =>
        controller.get(p) match {
          case th : DeclaredTheory =>
            th.getConstants.map(c => SimpleLink(c.path,OMS(link.path / c.name),source,target,link))
          case _ => Nil
        }
      case _ => Nil
    }

    override def toString = "StructureTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
      "\nusing " + link.path.toString + "\n" +
      translations.map(" - " + _.toString).mkString("\n")
  }

  case class InverseStructureTranslation(orig : StructureTranslation) extends LinkTranslation {
    val target = orig.source
    val source = orig.target
    val link = orig.link
    val translations = orig.translations map {
      case SimpleLink(from,OMS(to),_,_,_) => SimpleLink(to,OMS(from),source,target,link)
    }

    override def toString = "InverseStructureTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
      "\nusing " + link.path.toString + "\n" +
      translations.map(" - " + _.toString).mkString("\n")
  }

  var translations : List[Translation] = Nil
  var translationgroups : List[TranslationGroup] = Nil

  lazy val archives = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }
  lazy val alignments = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  def fromAlignment(a : FormalAlignment) : List[AlignmentTranslation] = {
    val src = archives.find(a.from.mmturi)
    val trg = archives.find(a.to.mmturi)
    if (src.isEmpty || trg.isEmpty) return Nil
    a match {
      case al : SimpleAlignment =>
        val ret = if (a.invertible) List(SimpleAlignmentTranslation(al,src,trg),
          SimpleAlignmentTranslation(al.reverse.asInstanceOf[SimpleAlignment],trg,src))
        else List(SimpleAlignmentTranslation(al,src,trg))
        translations = (ret ::: translations).distinct
        ret
      case al : ArgumentAlignment =>
        val ret = if (a.invertible) List(ArgumentAlignmentTranslation(al,src,trg),
          ArgumentAlignmentTranslation(al.reverse.asInstanceOf[ArgumentAlignment],trg,src))
        else List(ArgumentAlignmentTranslation(al,src,trg))
        translations = (ret ::: translations).distinct
        ret
    }
  }

  def fromLink(v : DeclaredLink) : Option[(LinkTranslation,LinkTranslation)] = {
    val (from, to) = (v.from,v.to) match {
      case (OMMOD(p : MPath),OMMOD(q : MPath)) => (p,q)
      case _ => return None
    }
    val src = archives.find(from)
    val trg = archives.find(to)
    if (src.isEmpty || trg.isEmpty) return None
    val ret = v match {
      case vw : DeclaredView =>
        (ViewTranslation(vw,src,trg),InverseViewTranslation(vw,trg,src))
      case str : DeclaredStructure =>
        val tr = StructureTranslation(str,src,trg)
        (tr,InverseStructureTranslation(tr))
    }

    add(ret._1)
    add(ret._2)
    Some(ret)
  }

  private def expandDefinition(p : GlobalName) : Option[Term] = controller.get(p) match {
    case c: FinalConstant if c.df.isDefined => Some(c.df.get)
    case _ => None
  }

  private case class State(visited : List[GlobalName], applied : List[Translation], usedgroups : List[TranslationGroup]) {
    def add(vs : GlobalName) = State(vs :: visited, applied,usedgroups)
    def add(vs : (GlobalName,List[GlobalName],Translation)) = State(vs._1 :: visited, vs._3 :: applied, usedgroups)
    def add(gr : (TranslationGroup,GlobalName,List[GlobalName],Translation)) =
      State(gr._2 :: visited, gr._4 :: applied, gr._1 :: usedgroups)

    def +(s : State) =State(visited,applied,s.usedgroups ::: usedgroups)
  }

  private def findApplicable(t : Term, state : State)(implicit target: FullArchive) : List[State] = {
    val usedgroups = state.usedgroups.collect{
      case g if g.isApplicable(t).nonEmpty => g.isApplicable(t)
    }.flatten.filter(p => !p._2.filter(!target.declares(_)).exists(state.visited.contains))

    if (usedgroups.nonEmpty) return usedgroups.map(u => state add u)

    val newgroups = translationgroups.collect{
      case gr if gr.isApplicable(t).nonEmpty => gr.isApplicable(t).map(p => (gr,p._1,p._2,p._3))
    }.flatten.filter(p => !p._3.filter(!target.declares(_)).exists(state.visited.contains))

    if (newgroups.nonEmpty) return newgroups.map(gr => state add gr)

    val applicables = translations.collect {
      case tr if tr.isApplicable(t).isDefined =>
        val ret = tr.isApplicable(t).get
        (ret._1,ret._2,tr)
    }.filter(p => !p._2.filter(!target.declares(_)).exists(state.visited.contains))
    applicables.map(p => state add p)
  }

  private def traverse(con : Context, state : State)(implicit target: FullArchive) : List[(Context,State)] = {
    val ret = traverse(state, con.variables.map(v => v.tp.get): _*)
    ret.map{
      case (l,st) => (Context(con.indices.map(i => VarDecl(con(i).name,Some(l(i)),None,None)):_*),st)
    }
  }

  private def traverse(state: State, terms : Term*)(implicit target: FullArchive) : List[(List[Term],State)] =
    if (terms.isEmpty) Nil else if (terms.length == 1) traverse(terms.head,state).map(p => (List(p._1),p._2)) else {
      val last = traverse(terms.last, state) map {
        case (tm, st) =>
          (List(tm), st)
      }
      terms.dropRight(1).foldRight(last)({
        case (arg,list) => list flatMap {
          case (ls,st) =>
            traverse(arg,state + st) map {
              case (tm,nst) => (tm :: ls,nst)
            }
        }
      })
    }

  private def traverse(t : Term, state : State)(implicit target: FullArchive) : List[(Term,State)] = t match {
    case OMV(name) => List((t,state))
    case OMS(p) =>
      if (target.declares(p)) List((t, state))
      else {
        val ways = findApplicable(t, state)
        if (ways.nonEmpty) ways.flatMap(s => traverse(s.applied.head.apply(t), s))
        else {
          val df = expandDefinition(p)
          if (df.isDefined) traverse(df.get, state)
          else Nil
        }
      }
    case _ =>
      val ways = findApplicable(t, state)
      if (ways.nonEmpty) ways.flatMap(s => traverse(s.applied.head.apply(t), s))
      else t match {
        case OMA(f, args) =>
          traverse(state,f :: args :_*) map {
            case (ls,st) => (OMA(ls.head,ls.tail),st)
          }
        case OMBINDC(binder,con,bodies) =>
          val nbinds = traverse(binder,state)
          val ncons = nbinds flatMap {
            case (tm,st) =>
              val conts = traverse(con,state + st)
              conts.map {
                case (ncon,nst) => (tm,ncon,nst)
              }
          }
          ncons flatMap {
            case (nbinder,ncon,st2) =>
              traverse(state + st2,bodies :_*) map {
                case (ls,nst) => (OMBINDC(nbinder,ncon,ls),nst)
              }
          }
        case UnknownOMLIT(s,tm) => traverse(tm,state) map {
          case (ntm,st) => (UnknownOMLIT(s,ntm),st)
        }
        case OMLIT(value,rt) => traverse(rt.synType,state) map {
          case (ntm,st) => (UnknownOMLIT(value.toString,ntm),st)
        }
        case OML(VarDecl(name,tp,df,not)) =>
          val tps = if (tp.isDefined) traverse(tp.get,state).map(p => (Some(p._1),p._2)) else List((None,state))
          val dfs = tps flatMap {
            case (tpOpt,st) => if (df.isDefined) traverse(df.get,state + st).map(p => (tpOpt,Some(p._1),p._2)) else List((tpOpt,None,st))
          }
          dfs map {
            case (tpOpt,dfOpt,nst) => (OML(name,tpOpt,dfOpt),nst)
          }
        case _ => Nil
      }
  }

  def apply(t : Term, target : FullArchive) : List[Term] = traverse(t,State(Nil,Nil,Nil))(target).map(_._1)

  def apply(t : Term, target : String) : List[Term] = apply(t,archives.getArchive(target).getOrElse(return Nil))

  def loadAlignments = {
    log("Getting Alignments from AlignmentServer...")
    logGroup {
      val als = alignments.getAll collect {
        case a: FormalAlignment => a
      }
      log(als.length + " Alignments found.")
      log("Adding Alignment Translations...")
      als foreach fromAlignment
      log("Done.")
    }
  }

  def loadLinks = {
    log("Collecting Links...")
    logGroup {
      val links = (controller.evaluator.evaluate(Paths(IsView)).asInstanceOf[ESetResult].h.toList :::
        controller.evaluator.evaluate(Paths(IsStructure)).asInstanceOf[ESetResult].h.toList).flatten.map(s =>
        Path.parse(s.toString)).map(p => Try(controller.get(p))) collect {
          case Success(l: DeclaredLink) => l
        }
      log(links.length + " Links found")
      log("Adding LinkAlignments...")
      links foreach fromLink
      log("Done.")
    }
  }

  def loadAll = {
    loadAlignments
    archives.getArchives.foreach(a => a.read)
    loadLinks
    log(translations.length + " (Simple) Translations and " + translationgroups.length + " Alignment groups present")
    log("Simple Translations:")
    logGroup{
      translations.foreach(t => log(t.toString))
    }
    log("Translation Groups:")
    logGroup{
      translationgroups.foreach(t => log(t.toString))
    }
  }

  def add(t : Translation) = translations = (t :: translations).distinct

  def add(t : TranslationGroup) = translationgroups = (t :: translationgroups).distinct

}
