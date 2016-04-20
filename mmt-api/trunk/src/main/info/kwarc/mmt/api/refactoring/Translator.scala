package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.{OMA, OMS, OMV, Term}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.symbols.FinalConstant

abstract class Translation {
  val source : FullArchive
  val target : FullArchive
  def isApplicable(t : Term) : Boolean
  protected def translate(t : Term, cont : Term => List[Term]) : List[Term]

  def apply(t : Term, cont : Term => List[Term] = t => List(t)) : List[Term] = {
    if (isApplicable(t)) translate(t,cont) else cont(t)
  }
}

abstract class AlignmentTranslation(alignment : FormalAlignment) extends Translation {
  require(alignment.to.mmturi.isInstanceOf[GlobalName])
  require(alignment.from.mmturi.isInstanceOf[GlobalName])
  val from = alignment.to.mmturi.asInstanceOf[GlobalName]
  val to = alignment.to.mmturi.asInstanceOf[GlobalName]
}

class Translator extends Extension {
  override def logPrefix = "Translator"

  override def start(args: List[String]) = {

  }

  private case class SimpleAlignmentTranslation(a : SimpleAlignment, source : FullArchive, target : FullArchive)
    extends AlignmentTranslation(a) {
    def isApplicable(t : Term) : Boolean = t match {
      case OMS(p) if p == from => true
      case _ => false
    }
    protected def translate(t : Term, cont : Term => List[Term]) : List[Term] = t match {
      case OMS(p) if p == from => List(OMS(to))
      case _ => Nil // can not ever occur
    }
  }

  private case class ArgumentAlignmentTranslation(align : ArgumentAlignment, source : FullArchive, target : FullArchive)
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

    def isApplicable(t : Term) : Boolean = t match {
      case OMS(p) if p == from => true
      case Application(OMS(p),args,_) if p == from => true
      case _ => false
    }
    private def reorder(ts : List[Term]) : List[Term] = {
      val max = align.arguments.maxBy(p => p._2)._2
      (1 to max).map(i => {
        val ni = align.arguments.find(p => p._2 == i).map(_._1)
        if (ni.isEmpty) OMV(LocalName("_")) // TODO implicit arguments
        else ts(ni.get)
      }).toList
    }
    protected def translate(t : Term, cont : Term => List[Term]) : List[Term] = t match {
      case OMS(p) if p == from => List(OMS(to))
      case Application(OMS(p),args,appls) if p == from => cont(Application(OMS(to),reorder(args),appls))
      case _ => Nil // can not ever occur
    }
  }

  var translations : List[Translation] = Nil
  lazy val archives = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }

  def fromAlignment(a : FormalAlignment) : Option[AlignmentTranslation] = {
    val src = archives.find(a.from.mmturi)
    val trg = archives.find(a.to.mmturi)
    if (src.isEmpty || trg.isEmpty) return None
    a match {
      case al : SimpleAlignment =>
        val ret = SimpleAlignmentTranslation(al,src.get,trg.get)
        translations = (ret :: translations).distinct
        Some(ret)
      case al : ArgumentAlignment =>
        val ret = ArgumentAlignmentTranslation(al,src.get,trg.get)
        translations = (ret :: translations).distinct
        Some(ret)
    }
  }
}
