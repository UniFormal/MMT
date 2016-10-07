package info.kwarc.mmt.focalize

import syntax._

import info.kwarc.mmt.api._
import frontend._
import documents._
import archives._
import modules._
import symbols._
import objects._
import utils._

import info.kwarc.mmt.lf._

class FocalizeImporter extends Importer {
   val key = "focalize-omdoc"
   def inExts = List("fcd")

   private val parseXML = new utils.XMLToScala("info.kwarc.mmt.focalize.syntax")

   def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
      val focdoc = try {
         parseXML(bf.inFile) match {
           case d: focdoc => d
           case _ => throw LocalError("unexpected root element")
         }
      } catch {
        case e: utils.ExtractError =>
          throw LocalError("error while parsing XML").setCausedBy(e)
      }
      val t = new Translator(this, controller, bf)
      val mmtdoc = t.applyDoc(focdoc)
      index(mmtdoc)
      BuildResult.fromImportedDocument(mmtdoc)
   }
}

/** keeps track of which theory a name is imported from */
abstract class NameInfo {
  val name: LocalName
}
case class Method(name: LocalName, from: MPath, inherited: Boolean) extends NameInfo
case class SpeciesParameter(name: LocalName) extends NameInfo
case class Variable(name: LocalName) extends NameInfo
case class GlobalInCurrentDoc(doc: DPath, name: LocalName) extends NameInfo
//global names can't be tracked in general because files not be processed in order
//case class Global(name: LocalName) extends NameInfo

class Translator(imp: FocalizeImporter, controller: Controller, bt: BuildTask) {

   /** keeps track of where names come from in a species
    *  global names are not tracked because we might anyway read files out of order
    *  bound variables are tracked separately by carrying the context as an implicit argument
    */
   private var docInfos: List[NameInfo] = Nil
   private var modInfos: List[NameInfo] = Nil
   private def findNameInfo(name: LocalName)(implicit c: Context) = {
     val names = c.reverseMap(v => Variable(v.name)) ::: modInfos ::: docInfos
     names.find(_.name == name)
   }
   private def applyByNameInfo(ln: LocalName)(implicit c: Context): Option[Term] = {
     findNameInfo(ln) map {
       case Method(_, from, _) => OMS(from ? ln) // from species (local or inherited)
       case _:SpeciesParameter => OMV(ln)
       case _:Variable => OMV(ln)
       case g:GlobalInCurrentDoc => OMMOD(g.doc ? ln)
     }
   }
   
   private def currentDoc = bt.narrationDPath
   // namespace is given by file name only; archive, folders, and file extension are ignored
   private def makeNamespace(file: String) = Focalize._base / file
   private def currentNamespace = makeNamespace(bt.inPath.stripExtension.segments.last)
   
   // **** documents ****  
  
   def applyDoc(d: focdoc): Document = {
     val focdoc(nam, decls) = d
     val doc = new Document(currentDoc, root = true)
     controller add doc
     decls foreach {
       case m: FocModule => applyMod(doc, m)
       case td: ToplevelDeclaration => applyTop(doc, td)
     }
     doc
   }
   
   // **** modules ****  

   def applyMod(doc: Document, m: FocModule) {
     implicit val c = Context.empty
     m match {
       case species(nam, pars, decls) =>
         try {
           val thy = new DeclaredTheory(currentNamespace, LocalName(nam.foc_name), Some(Focalize._path))
           controller.add(thy)
           controller.add(MRef(doc.path, thy.path))
           val parsM = pars.map {case parameter(kind, tp, n) =>
              val nM = LocalName(n.foc_name)
              val tpM = if (kind == parameter.collection) {
                applyCollSpec(tp)
              } else {
                applyCollSpec(tp)
              }
              modInfos ::= SpeciesParameter(nM)
              VarDecl(nM, Some(tpM), None, None)
           }
           thy.parameters = parsM
           decls foreach {d =>
             val (ii, dM) = applyDec(thy, d)
             dM foreach {d =>
               insertNotation(d)
               controller add d
             }
             ii foreach {modInfos ::= _}
           }
         } finally {
           modInfos = Nil
         }
       case collection(nam, impl, _) =>
         val to = TheoryExp.empty
         val vw = new DeclaredView(currentNamespace, LocalName(nam.foc_name), applyCollSpec(impl), to, false)
         controller.add(vw)
         controller.add(MRef(doc.path, vw.path))
         // TODO how to handle collections? body repeats all inferred declarations         
       case _ => // open, load etc. ignored
     }
   }
   
   
   def applyTop(doc: Document, d: ToplevelDeclaration) {
     // TODO declarations must be added to doc itself, not to a dummy theory in it
     val thy: DeclaredTheory = new DeclaredTheory(currentNamespace, LocalName("global"), Some(Focalize._path))
     controller add thy
     def add(c: Declaration) {
       controller add c
       docInfos ::= GlobalInCurrentDoc(doc.path, c.name)
     }
     def addTypeConst(c: Declaration) {
       insertNotation(c)
       add(c)
     }
     def addTermConst(c: Declaration, tpArgs: Int, explArgs: Int) {
       insertNotation(c, tpArgs, explArgs)
       add(c)
     }
     implicit val c = Context.empty
     d match {
       case d: FocModule =>
       case d: FocDeclaration =>
         val (_, Some(dM)) = applyDec(thy, d)
         addTypeConst(dM)
       case global_fun(n, tp, df) =>
         val (_, Some(dM)) = applyDec(thy, definition(NamedDecl(n, None, None), tp, df))
         addTypeConst(dM)
       case concrete_type(n, pars, df) =>
         val cont = pars map {case param_type(tvar(n)) => VarDecl(LocalName(n), Some(OMS(Focalize.tp)), None, None)}
         def parTp(t: Term) = Pi(cont, t)
         val tpM = parTp(OMS(Focalize.tp))
         def tpConst(df: Option[Term], rl: String) = Constant(thy.toTerm, LocalName(n), Nil, Some(tpM), df, Some(rl))
         df match {
            case external_type() :: Nil =>
              addTypeConst(tpConst(None, "external_type"))
            case external_type() :: _ =>
              throw imp.LocalError("ill-formed XML: children left after external type")
            case alias(t):: Nil =>
              val dfM = Lambda(cont, applyType(t)(c++cont))
              addTypeConst(tpConst(Some(dfM), "alias"))
            case alias(_) :: _ => throw imp.LocalError("ill-formed XML: children left after alias")
            case (_:record_label_and_type) :: _ =>
              val tp = tpConst(None, "record")
              addTypeConst(tp)
              val recType: Term = tp.toTerm
              df foreach {
                case record_label_and_type(n, ntp) =>
                  val projType = parTp(Apply(OMS(Focalize.expr), ApplySpine(OMS(Focalize.fun), recType, applyType(ntp)(c++cont))))
                  val projConst = Constant(thy.toTerm, LocalName(n), Nil, Some(parTp(applyTypeLifted(ntp))), None, Some("record_field"))
                  addTermConst(projConst, cont.length, 1)
                case _ => throw imp.LocalError("ill-formed XML: non-constr children after constr child")
              }
            case (_:constr) :: _ =>
              val tp = tpConst(None, "inductive")
              val indType: Term = tp.toTerm
              add(tp)
              df foreach {
                case constr(n, ntp) =>
                  val conType = parTp(Apply(OMS(Focalize.expr), ApplySpine(OMS(Focalize.fun), applyType(ntp)(c++cont), indType)))
                  val conConst = Constant(thy.toTerm, LocalName(n), Nil, Some(conType), None, Some("constructor"))
                  val numExplArgs = ntp match {
                    case sum_args(as) => as.length
                    case _ => throw imp.LocalError("ill-formed XML: arguments of constructor are not sum_args")
                  }
                  addTermConst(conConst, cont.length, numExplArgs)
                case _ => throw imp.LocalError("ill-formed XML: non-constr children after constr child")
              }
            case Nil => throw imp.LocalError("ill-formed XML: no children in concrete type") // could be empty record or empty inductive type
         }
     }
   }
   
   // **** declarations ****
   
   def applyDec(mod: DeclaredModule, d: FocDeclaration): (Option[Method], Option[Declaration]) = {
     d match {
       case signature(name, tp) if name.foc_name == "rep" =>
         // work around for awkward (faulty?) export of carrier definition
         return applyDec(mod, carrier(name, tp))
       case _ =>
     }
     val namePref = mod match {
       case t: DeclaredTheory => LocalName.empty
       case v: DeclaredView => v.from match {
         case OMPMOD(p,_) => LocalName(p)
       }
     }
     val inhInfo = d match {
       case nd: NamedDeclaration =>
         nd.name._history.map(_.origin) map {case (from, inh) =>
           Method(LocalName(nd.name.foc_name), applyModId(from), inh)
         }
       case _ => None
     }
     // skip inherited declarations
     // TODO redefinitions (currently not exported)
     inhInfo foreach {i => if (i.inherited) return (inhInfo, None)}
     implicit val c = Context.empty
     val dM = d match {
       case inherits(from) =>
         val (mp, args) = applyCollSpecAux(from)
         Include(mod.toTerm, mp, args)
       case signature(n, tp) =>
         Constant(mod.toTerm, LocalName(n.foc_name), Nil, Some(applyTypeLifted(tp)), None, Some("signature"))
       case property(n, prop) =>
         Constant(mod.toTerm, LocalName(n.foc_name), Nil, Some(applyPropLifted(prop)), None, Some("property"))         
       case letprop(n, pars, p) =>
         val cont = pars map {case param_prop(n,tp) => VarDecl(LocalName(n), Some(applyTypeLifted(tp)), None, None)}
         val tp = Pi(cont, OMS(Focalize.prop))
         val df = Lambda(cont, applyProp(p)(c++cont))
         Constant(mod.toTerm, LocalName(n.foc_name), Nil, Some(tp), Some(df), Some("letprop"))
       case carrier(_, tp) =>
         Constant(mod.toTerm, LocalName(Focalize._path) / Focalize.self.name, Nil, None, Some(applyType(tp)), Some("carrier"))
       case definition(n, tp, df) =>
         val dfM = df map applyExpr
         Constant(mod.toTerm, namePref / n.foc_name, Nil, Some(applyTypeLifted(tp)), dfM, Some("definition"))
       case theorem(n, prop, pf) =>
         val pfM = Some(applyProof(pf))
         Constant(mod.toTerm, namePref / n.foc_name, Nil, Some(applyPropLifted(prop)), pfM, Some("theorem"))
     }
     (inhInfo, Some(dM))
   }
  
   // **** expressions ****
   
   def applyCollSpec(e: CollectionOrSpeciesExpr)(implicit c: Context) = {
     val (mp, args) = applyCollSpecAux(e)
     ApplySpine(OMMOD(mp), args:_*)
   }
   private def applyCollSpecAux(e: CollectionOrSpeciesExpr)(implicit c: Context): (MPath, List[Term]) = {
     def resolveName(f: String, n: String) =
       if (f.nonEmpty) applyModId(f, n) else currentNamespace ? LocalName(n)
     e match {
       case atom(_, file, name) =>
         (resolveName(file, name), Nil)
       case app(op, args) =>
         val argsM = args map {case param(infile, name) =>
           if (name == "Self") {
             OMCOMP()// identity morphism of containing theory to instantiate parameters with corresponding imports into current theory
           } else {
             val ln = LocalName(name)
             applyByNameInfo(ln).getOrElse {
               // default to global collection
               if (infile.isEmpty) {
                 throw imp.LocalError("no file in collection reference: " + e)
               }
               OMMOD(applyModId(foc_name(infile,name)))
             }
           }
         }
         (resolveName(op.infile, op._name), argsM)
     }
   }

   /** toplevel translation of types lifts to LF types */
   private def applyTypeLifted(e: TypeExpr)(implicit c: Context) = Apply(OMS(Focalize.expr), applyType(e))

   def applyType(e: TypeExpr)(implicit c: Context): Term = {
     e match {
       case atom(order, file, name) =>
         val t = OMMOD(applyModId(file, name))
         if (order == "first")
           t
         else // order == "high"
           OMM(t, OMS(Focalize.self)) // self of a collection
       case self() => OMS(Focalize.self) // self of the current species
       case tvar(n) => OMV(n)
       case prm(_, args, op) => ApplySpine(OMMOD(applyModId(op)), args map applyType :_*)
       case fct(from, to) => ApplySpine(OMS(Focalize.fct), applyType(from), applyType(to))
       case prod(args) => ApplySpine(OMS(Focalize.prod), args map applyType :_*)
       case sum_args(args) => applyType(prod(args))
     }
   }
   
   /** auxiliary function for translating propositional formulas */
   private def propAux(op: GlobalName, args: Proposition*)(implicit c: Context) = ApplySpine(OMS(op), args map applyProp:_*)
   /** auxiliary function for translating quantified formulas */
   private def quanAux(op: GlobalName, v: foc_name, tp: TypeExpr, bd: Proposition)(implicit c: Context) = {
     val cont = Context(VarDecl(applyName(v), Some(applyTypeLifted(tp)), None, None))
     Apply(OMS(op), Lambda(cont, applyProp(bd)(c++cont)))
   }
     
   /** toplevel translation of propositions lifts to LF types */
   private def applyPropLifted(e: Proposition)(implicit c: Context) = try {
     Apply(OMS(Focalize.proof), applyProp(e))
   } catch {case exc: imp.LocalError =>
     throw imp.LocalError("error while translating " + e).setCausedBy(exc)
   }

   def applyProp(e: Proposition)(implicit c: Context): Term = {
     e match {
       case e: Expression => applyExpr(e)
       case paren_logical_expr(e) => applyProp(e)
       case and(l,r) => propAux(Focalize.and, l, r)
       case or(l,r)  => propAux(Focalize.or, l, r)
       case not(l)   => propAux(Focalize.not, l)
       case implies(l,r) => propAux(Focalize.implies, l, r)
       case equiv(l,r) => propAux(Focalize.equiv, l, r)
       case all(v, t, b) => quanAux(Focalize.all, v, t, b)
       case ex(v, t, b) => quanAux(Focalize.ex, v, t, b)
     }
   }
   
   /** auxiliary function for translating propositional formulas */
   private def exprAux(op: GlobalName, props: List[Proposition], args: Expression*)(implicit c: Context) =
     ApplySpine(OMS(op), (props map applyProp) ++ (args.toList map applyExpr):_*)
   // TODO booleans must be lifted/cast to propositions
   def applyExpr(e: Expression)(implicit c: Context): Term = {
      e match {
        case paren_expr(e) => applyExpr(e)
        case id: identifier => applyDeclId(id)
        case symbol(math, id) => applyExpr(id)
        case if_expr(i, t, e) => exprAux(Focalize.if_expr, List(i), t, e)
        case fun(varnames, bd) =>
          val cont = varnames map {case foc_name(_,n) => VarDecl(LocalName(n), ???, None, None)}
          Apply(OMS(Focalize.fun), Lambda(cont, applyExpr(bd)(c++cont)))
        case application(fun, args) => exprAux(Focalize.app, Nil, fun :: args:_*)          
        case tuple_expr(args) => exprAux(Focalize.tuple, Nil, args:_*)
        case record_expr(fields) =>
          val fieldsM = fields map {case RecordField(n, e) =>
             (applyModId(n._name), applyExpr(e))
          }
          throw imp.LocalError("translation of record expressions not implemented yet") //TODO record expressions and pattern-matching do not actually occur in export yet
        case record_access_expr(r, proj) =>
          // reduce to application of the projection produced by elaborating the toplevel named record
          applyExpr(application(proj, List(r)))
        case int(v) => Focalize.intLiterals(v)
      }
   }
   
   def applyProof(pf: ProofExpr)(implicit c: Context): Term = {
     pf match {
       case proof() => OMS(Focalize.omittedProof)
     }
   }

   // **** identifiers ****
   
   def applyName(fn: foc_name): LocalName = LocalName(fn._name)
   
   def applyModId(file: String, name: String): MPath = {
     if (file.isEmpty)
       throw imp.LocalError("missing or empty infile attribute in foc_name with name " + name)
     makeNamespace(file) ? name
   }
   def applyModId(fn: foc_name): MPath = applyModId(fn.infile, fn._name)
   
   def applyDeclId(id: identifier)(implicit c: Context): Term = {
     val ln = applyName(id._name)
     id.of_species match {
       case Some(cn) => OMS(applyModId(cn) ? ln) // from collection cn
       case None => applyByNameInfo(ln).getOrElse {
         // defaulting to global name, only case where there may be type arguments
         val args = id.poly_args map applyType
         val op = OMMOD(applyModId(id._name)) // TODO global names must be identified differently
         ApplySpine(op, args:_*)
       }
     }
   }
   
   // **** notations ****

   /** generate and add mixfix notation according to spaces in constant name */
   private def insertNotation(c: Declaration) {
     val n = c.name.toString
     val delims = if (n.startsWith("(") && n.endsWith(")") && n.contains(' ')) {
       Some(utils.stringToList(n.substring(1,n.length-1)))
     } else
       None
     val ds = delims getOrElse {
       return
     }
     // TODO
   }

   /** generate and add prefix notation */
   private def insertNotation(c: Declaration, numImplArgs: Int, numExplArgs: Int) {
     //TODO
   }
   
}