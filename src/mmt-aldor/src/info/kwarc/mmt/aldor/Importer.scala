package info.kwarc.mmt.aldor

import info.kwarc.mmt.api._
import utils._
import documents._
import modules._
import symbols._
import objects._

import info.kwarc.mmt.lf._

object Aldor {
  def ns = DPath(URI("http://www.aldor.org"))
  def lisp = ns ? "Lisp"
  def path = ns ? "Aldor"
}

class AldorImporter extends archives.Importer {
  def key = "aldor-omdoc"
  def inExts = List("axy")

  // temporary to determine the needed names
  private var lispnames = new scala.collection.mutable.HashSet[String]()
  private var aldornames = new scala.collection.mutable.HashSet[String]()

  def importDocument(bt: archives.BuildTask, index: Document => Unit) = {
    val sexp = SExpression.parse(File.read(bt.inFile), false)
    val declsS = sexp.asList(0).asList
    val declsA = declsS flatMap AParser.declarations
    val dpath = bt.narrationDPath
    val contentBase = DPath(bt.archive.narrationBase/"content" )
    val doc = new Document(dpath)
    controller.add(doc)
    var foundMods: List[MPath] = Nil
    declsA foreach {
      case AConstant(n,None,pars,_,Some(NormalizedCategory(ds)),_,_) =>
        val parsM = translateBindings(pars)
        val thy = Theory(contentBase,LocalName(n.name),Some(Aldor.path),ContextContainer(parsM))
        foundMods ::= thy.path
        controller.add(MRef(dpath,thy.path,true))
        controller.add(thy)
        ds.foreach {
          case AConstant(n,cO,pars,tO,dO, doc, dflt) =>
            val parsM = translateBindings(pars)
            val cM = cO map translateExpr
            // the condition of a constant becomes an extra argument that creates an implicit proof obligation when the constant is used
            val ctM = tO map {t =>
              val tM = translateExpr(t)
              cM match {
                case Some(c) => Pi(OMV.anonymous,c,Pi.applyOrBody(parsM,tM))
                case None => tM
              }
            }
            val cdM = dO map {d =>
              val dM = translateExpr(d)
              cM match {
                case Some(c) => Lambda(OMV.anonymous,c,Lambda.applyOrBody(parsM,dM))
                case None => dM
              }
            }
            val nM = translateId(n)
            val con = Constant(OMMOD(thy.path),nM,Nil,ctM,cdM,None)
            if (dflt && thy.declares(nM)) {
              // add default definition of previously added constant
              val c = thy.get(nM).asInstanceOf[Constant]
              c.dfC.analyzed = cdM
            } else {
              controller.add(con)
            }
          case AInclude(cO,f) =>
            val (n,args) = getAtomicTheory(f)
            val homeThy = cO match {
              case None =>
                thy
              case Some(c) =>
                // we add a nested theory with an axiom for c that is the codomain of the include
                val cName = LocalName(c.hashCode().toString)
                val cThy = Theory(contentBase,thy.name/cName,Some(Aldor.path))
                val nm = new NestedModule(thy.toTerm, cName, cThy)
                controller.add(nm)
                val cAx = Constant(cThy.toTerm, LocalName("test"), Nil, Some(translateExpr(c)), None, None)
                controller.add(cAx)
                cThy
            }
            val incl = Include(homeThy.toTerm,Aldor.ns ? n,args map translateExpr)
            controller.add(incl)
        }
      case AConstant(n, None, pars, Some(cat), Some(AAdd(e,ds)), _, _) =>
        val domainIncludes = NormalizedCategory.flatten(cat) match {
          case AWith(_, ds) =>
            ds.collect {
              case AInclude(None,cat) => cat
              // case _ => throw LocalError("unsupported declaration in category of domain")
            }
          case e =>
            List(e)
        }
        val domainIncludesM = domainIncludes map {i =>
          val (n,as) = getAtomicTheory(i)
          (Aldor.ns ? n,as map translateExpr)
        }
        val codomain = ComplexTheory(translateBindings(pars))
        if (pars.length == 1) println("parameter: " + pars.head.tp) else println("other parameters")
        val vw = View(contentBase,LocalName(n.name),TUnion(domainIncludesM),codomain,false)
        foundMods ::= vw.path
        controller.add(MRef(dpath,vw.path,true))
        controller.add(vw)
        val initialIncludes = if (e == ANil) Nil else List(AInclude(None,e))
        (initialIncludes:::ds).foreach {
          case c: AConstant =>
            val ca = ConstantAssignment(vw.toTerm, translateId(c.name), Nil, c.df map translateExpr)
            controller.add(ca)
          case i: AInclude =>
            val ia = LinkInclude(vw.toTerm, vw.path, translateExpr(i.cat))
              println("included morphism: " + ia)
            // controller.add(ia)
        }
      case c:AConstant =>
        // other toplevel constants are less useful for the theory graph, so we skip them for now
        log("skipping constant " + c.name.name)
    }
    index(doc)
    archives.BuildSuccess(Nil, foundMods map {p => archives.LogicalDependency(p)})
  }

  def translateId(a:AId) = {
    val nM = LocalName(a.name)
    a.typeHash match {
      case None => nM
      case Some(th) => nM / th.toString
    }
  }

  def translateDecl(d: ADeclaration) = d match {
    case AConstant(n,None,Nil,tO,dO,_,_) =>
      OML(LocalName(n.name),tO map translateExpr,dO map translateExpr)
  }

  def translateExpr(e: AExpression): Term = {
    e match {
      case AId("%",_,_) => OMS(Aldor.ns ? "Category" ? "carrier") // %
      case AId("Rep",_,_) => OMS(Aldor.ns ? "Category" ? "carrier") // Rep refers to %
      case AId(n,None,None) => OMV(LocalName(n))
      case id @ AId(n,Some(kind),Some(_)) =>
        val nM = translateId(id)
        kind match {
          case "Param" => OMV(LocalName(n))
          case "LexConst" => OMV(LocalName(n))
          case "LexVar" => OMV(LocalName(n))
          case "Extend" => OMS(Aldor.ns ? "???" ? nM) // TODO
          case "Export" => OMS(Aldor.ns ? "???" ? nM) // TODO
          case "Import" => OMS(Aldor.ns ? "???" ? nM) // TODO
          case "Builtin" =>
            // if (!aldornames.contains(n))  println("aldor: " + n)
            aldornames += n
            OMS(Aldor.path ? n)
          case "Foreign" =>
            // if (!lispnames.contains(n))  println("aldor: " + n)
            aldornames += n
            OMS(Aldor.lisp ? n)
          case "Label" => OMV(LocalName(n))
        }
      case AOperator(op, args) => OMA(OMS(Aldor.path?op), args map translateExpr)
      case ANum(n) => uom.OMLiteral.OMI(n)
      case APi(bs, df) => Pi(translateBindings(bs), translateExpr(df))
      case ALambda(bs, _, bd) => Lambda(translateBindings(bs), translateExpr(bd))
      case AApply(f,as) => ApplySpine(translateExpr(f), as map translateExpr:_*)
      case AString(s) => uom.OMLiteral.OMSTR(s)
      case AForeign(n, tp) => OMS(Aldor.lisp ? n)
      case ANil => OMS(Aldor.path ? "nil")
      case ADeclare(n,t,dO) => OML(LocalName(n), Some(translateExpr(t)), dO map translateExpr)
      case AAssign(n,_,d) => OMA(OMS(Aldor.path?"assign"), List(OML(LocalName(n)), translateExpr(d)))
      case ARecord(decls) => OMA(OMS(Aldor.path?"record"), decls map translateDecl)
      case AWith(left, decls) => OMA(OMS(Aldor.path?"category"), translateExpr(left) :: (decls map translateDecl))
      case AAdd(left, decls) => OMA(OMS(Aldor.path?"domain"), translateExpr(left) :: (decls map translateDecl))
      case AJoin(mods) => OMA(OMS(Aldor.path?"join"), mods map translateExpr)
    }
  }
  def translateBindings(bs: List[ABinding]): Context = {
    bs map {b =>
      val nM = b.name match {
        case None => OMV.anonymous
        case Some(n) => LocalName(n)
      }
      VarDecl(nM, None, Some(translateExpr(b.tp)), b.df map translateExpr, None)
    }
  }

  private def getAtomicTheory(t: AExpression) = t match {
    case AId(n,_,_) => (n,Nil)
    case AApply(AId(n,_,_),as) => (n,as)
    case e =>
      throw LocalError("unknown atomic theory: " + e)
  }

}

