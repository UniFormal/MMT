package info.kwarc.mmt.aldor

import info.kwarc.mmt.api._
import utils._
import documents._
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}
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

  // Aldor export does not allow disambiguating names perfectly, so we use two passes with a persistent name map
  private val nameMap = new scala.collection.mutable.HashMap[AId,ContentPath]
  private var firstPass = true
  // temporary, see below
  // private val aldornames = new scala.collection.mutable.HashSet[String]

  override def clear {nameMap.clear; firstPass = true}

  // We store the base of the current library here for simplicity.
  // This means we must never translate two different libraries at the same time.
  private var contentBase: DPath = null
  
  def importDocument(bt: archives.BuildTask, index: Document => Unit,rel:ULOStatement => Unit) = {
    if (nameMap.nonEmpty) firstPass = false
    val sexp = SExpression.parse(File.read(bt.inFile), false)
    val declsS = sexp.asList(0).asList
    val declsA = declsS flatMap AParser.declarations
    val dpath = bt.narrationDPathOMDoc
    contentBase = DPath(bt.archive.narrationBase/"content" )
    val doc = new Document(dpath)
    controller.add(doc)
    def addModule(n: AId, mod: Module) {
      if (firstPass && nameMap.isDefinedAt(n)) {
        // this happens for domain extensions, but it actually only comes up about 5 times for relatively unimportant modules
        log("module name clash: " + n + " --- " + mod.path)
      }
      nameMap(n) = mod.path
      controller.add(MRef(dpath,mod.path,true))
      controller.add(mod)
    }
    def doTheory(n: AId, pars: List[ABinding], ds: List[ADeclaration], withCatInclude: Boolean) {
      val parsM = translateBindings(pars)
      val thy = Theory(contentBase,LocalName(n.name),Some(Aldor.path),ContextContainer(parsM))
      addModule(n, thy)
      val hasSomeInclude = ds.exists {
        case AInclude(None, _) => true
        case _ => false
      }
      if (withCatInclude && !hasSomeInclude) {
        // include the special theory Category for theories representing categories, unless it is already included anyway
        val catIncl = Include(thy.toTerm,Aldor.ns ? "Category",Nil)
        controller.add(catIncl)
      }
      // calculate the overloaded names, which require disambiguation
      val names = ds.flatMap {
        case c: AConstant if !c.default => List(c.id.name)
        case _ => Nil
      }
      val overloaded = names.distinct.filter(n => names.count(_==n) > 1)
      ds.foreach {
        case AConstant(n,cO,cPars,tO,dO, doc, dflt) =>
          val cParsM = translateBindings(cPars)
          val cM = cO map translateExpr
          // the condition of a constant becomes an extra argument that creates an implicit proof obligation when the constant is used
          val ctM = tO map {t =>
            val tM = translateExpr(t)
            val parTp = FunType(cParsM,tM)
            cM match {
              case Some(c) => Arrow(c, parTp)
              case None => parTp
            }
          }
          val cdM = dO map {d =>
            val dM = translateExpr(d)
            val parDf = Lambda.applyOrBody(cParsM,dM)
            cM match {
              case Some(c) => Lambda(OMV.anonymous,c,parDf)
              case None => parDf
            }
          }
          // add qualifier if this name is overloaded
          val nM = if (overloaded.contains(n.name)) translateId(n) else LocalName(n.name)
          val con = Constant(OMMOD(thy.path),nM,Nil,ctM,cdM,None)
          if (dflt && thy.declares(nM)) {
            // add default definition of previously added constant
            val c = thy.get(nM).asInstanceOf[Constant]
            c.dfC.analyzed = cdM
          } else {
            // this causes a name clash once when reciprocal in UnivariateTaylorSeriesType is declared twice with two different conditions
            try {controller.add(con)}
            catch {case e:AddError =>
              log("skipping duplicate name for " + con.name)
            }
            if (firstPass && nameMap.isDefinedAt(n))
              throw LocalError("constant name clash: " + n + " --- " + con.path)
            nameMap(n) = con.path
          }
        case AInclude(cO,f) =>
          val (n,args) = getAtomicTheory(f)
          val homeThy = cO match {
            case None =>
              thy
            case Some(c) =>
              // we add a nested theory with an axiom for c that is the codomain of the include
              // nice name for the most important special cases
              def pickName(c: AExpression): String = c match {
                case AOperator("Test", List(AOperator("Has", List(_,AId(n,_,_))))) => n
                case AOperator("And", es) => pickName(es.head)
                case _ => c.hashCode().toString
              }
              val cLocalName = LocalName(pickName(c))
              val cThy = Theory(contentBase,thy.name/cLocalName,Some(Aldor.path))
              val nm = new NestedModule(thy.toTerm, cLocalName, cThy)
              controller.add(nm)
              val cAx = Constant(cThy.toTerm, LocalName("condition"), Nil, Some(translateExpr(c)), None, None)
              controller.add(cAx)
              cThy
          }
          val incl = Include(homeThy.toTerm, contentBase ? n,args map translateExpr)
          controller.add(incl)
      }
    }
    declsA foreach {
      case AConstant(n,None,pars,_,Some(NormalizedCategory(ds)),_,_) =>
        // category with declarations in definiens --> closed theory
        doTheory(n, pars, ds, true)
      case AConstant(n, None, pars, Some(NormalizedCategory(ds)),None,_,_) =>
        // abstract domain with declarations in type and no definiens --> open theory
        doTheory(n, pars, ds, false)
      case AConstant(n, None, pars, Some(cat), Some(AAdd(e,ds)), _, _) =>
        val codomain = if (pars.isEmpty)
          OMMOD(Aldor.path)
        else
          ComplexTheory(translateBindings(pars))
        val domainIncludes = NormalizedCategory.flatten(cat) match {
          case AWith(_, ds) =>
            val includes = ds.collect {
              case AInclude(None,i) => i
            }
            val includesOnly = includes.length == ds.length
            if (includesOnly) {
              // only includes: use each as domain
              includes
            } else {
              // otherwise: materialize the domain
              val typeName = n.copy(name=n.name+".Type")
              log("materializing domain: " + typeName)
              doTheory(typeName, pars, ds, true)
              val typeExpr = if (pars.isEmpty) typeName else {
                val namedPars = pars.collect {
                  case ABinding(Some(p),_,_) => p
                }
                AApply(typeName, namedPars.map(p => AId(p, None, None)))
              }
              List(typeExpr)
            }
          case e =>
            List(e)
        }
        val domainIncludesM = domainIncludes map {i =>
          val (n,as) = getAtomicTheory(i)
          (contentBase ? n,as map translateExpr)
        }
        val initialInclude = if (e == ANil) new TermContainer() else TermContainer(translateExpr(e))
        val vw = View(contentBase,LocalName(n.name),TUnion(domainIncludesM),codomain, initialInclude, false)
        addModule(n,vw)
        ds.foreach {
          case c: AConstant =>
            val cN = nameMap.get(c.id).map{case p: GlobalName => p.toLocalName}.getOrElse(translateId(c.id))
            val ca = ConstantAssignment(vw.toTerm, cN, Nil, c.df map translateExpr)
            controller.add(ca)
          case i: AInclude =>
            // this happens for one weird include that has a condition but not body
            log("skipping included morphism")
        }
      case c:AConstant =>
        // other toplevel constants are less useful for the theory graph, so we skip them for now
        // but we put them in the nameMap so that references to them are at least correct
        val n = c.id.name
        val p = if (bt.inPath == FilePath("sal_lang.axy") ||bt.inPath == FilePath("sal_base.axy"))
          // many of these can be seen as built-in constants, especially the ones from sal_lang.axy
          Aldor.path ? n
        else {
          // this happens for about 10 toplevel constants that are not categories or domains
          log("skipping constant " + n)
          contentBase ? n
        }
        nameMap(c.id) = p
    }
    index(doc)
    archives.BuildSuccess(Nil, Nil)
  }

  def translateId(a:AId) = {
    val nM = LocalName(a.name)
    a.typeHash match {
      case None => nM
      case Some(th) => nM / th.toString
    }
  }

  def translateDecl(d: ADeclaration) = d match {
    case AConstant(n,None,bds,tO,dO,_,_) =>
      val bdsM = translateBindings(bds)
      OML(LocalName(n.name),tO map {t => Pi.applyOrBody(bdsM, translateExpr(t))},
                            dO map {d => Lambda.applyOrBody(bdsM, translateExpr(d))})
  }

  def translateExpr(e: AExpression): Term = {
    e match {
      case AId("%",_,_) => OMS(Aldor.ns ? "Category" ? "carrier") // %
      case AId("Rep",_,_) => OMS(Aldor.ns ? "Category" ? "carrier") // Rep refers to %
      case AId(n,None,None) => OMV(LocalName(n))
      case AQualify(id, parent) => OMA(OMS(Aldor.path?"Qualify"), List(translateExpr(id), translateExpr(parent)))
      case id @ AId(n,Some(kind),Some(_)) =>
        kind match {
          case "Param" => OMV(LocalName(n))
          case "Label" => OMV(LocalName(n))
          case "Builtin" =>
            println("aldor: " + n)
            OMS(Aldor.path ? n)
          case "Foreign" =>
            println("lisp: " + n)
            OMS(Aldor.lisp ? n)
          // unclear when these come up exactly, but the name map has all constants that we know of
          case "LexConst" | "LexVar" | "Extend" | "Export" | "Import" =>
            nameMap.get(id) match {
              case Some(p) => OMID(p)
              case None => OMV(id.name)  // default to variable
            }
        }
      case AOperator(op, args) =>
        // use this for checking which built-in names occur; these should be declared manually in the MMT theory for Aldor in the archive aldor/language
        /* if (!aldornames.contains(op)) {
          println("aldor: " + op)
          aldornames += op
        } */
        OMA(OMS(Aldor.path?op), args map translateExpr)
      case ANum(n) => uom.OMLiteral.OMI(n)
      case APi(bs, df) => FunType(translateBindings(bs), translateExpr(df))
      case ALambda(bs, _, bd) => Lambda(translateBindings(bs), translateExpr(bd))
      case AApply(f,as) => ApplySpine(translateExpr(f), as map translateExpr:_*)
      case AString(s) => uom.OMLiteral.OMSTR(s)
      case AForeign(n, tp) =>  OMS(Aldor.lisp ? n)
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
      println("unknown atomic theory: " + e)
      ("",Nil)
      //throw LocalError("unknown atomic theory: " + e)
  }

}

