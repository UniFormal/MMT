package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{CovariantParameterPreprocessor, Hasher, Preprocessor}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.lf.hollight.HOLLight.vfhoas
import info.kwarc.mmt.lf._
import utils._
import objects.Conversions._

import scala.collection.mutable

object PVSTheory {
   val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
   val nasapath = DPath(URI.http colon "shemesh.larc.nasa.gov") / "fm" / "ftp" / "larc" / "PVS-library"
   val thname = "PVS"
   val thpath = rootdpath ? thname//Path.parseM("http://pvs.csl.sri.com/?PVS",NamespaceMap.empty) // rootdpath ? thname
   val preludepath = rootdpath ? "Prelude" //Path.parseM("http://pvs.csl.sri.com/?Prelude",NamespaceMap.empty) //rootdpath ? "Prelude"
   val natlit = thpath ? "NatLit"
   class sym(s: String) {
     val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
     val nasapath = DPath(URI.http colon "shemesh.larc.nasa.gov") / "fm" / "ftp" / "larc" / "PVS-library"
     val thname = "PVS"
     val thpath = rootdpath ? thname
      val path = thpath ? s
      val term = OMS(path)
   }

  val vfhoas = ViewFinderHOAS(tp.path,expr.path,pvslambda.path,pvsapply.path,fun_type.path)
  private val paramelim = new Preprocessor {
    private lazy val syms = getSyms(thpath)
    private val symmap : mutable.HashMap[MPath,List[GlobalName]] = mutable.HashMap.empty
    private def getSyms(mp : MPath) : List[GlobalName] = symmap.getOrElseUpdate(mp,{
      val th = controller.getAs(classOf[Theory],mp)
      th.getIncludes.flatMap(getSyms).distinct ::: th.getConstants.map(_.path)
    })

    private val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine(OMS(p),args) if !syms.contains(p) =>
          OMS(p)
        case ApplySpine(OMS(pvsapply.path), List(_,tpf,nf, tuple_expr(args))) =>
          args.foldLeft(traverse(nf))((f,p) =>
            ApplySpine(OMS(pvsapply.path),Hasher.Complex(p._2),Hasher.Complex(tpf),f,traverse(p._1))
          )
        case fun_type(tuple_type(ls),b) =>
          ls.foldRight(traverse(b))((tp,r) => fun_type(tp,r))
          // Some(unapply(nf).map(p => (p._1, p._2 ::: a :: Nil)).getOrElse((nf, List(a))))
        case _ =>
          Traverser(this,t)
      }
    }
    override protected def doTerm(tm: Term): Term = trav(tm,())
  }
  private val notccs = new Preprocessor {
    override def apply(th : Theory) : Theory = {
      val nth = new Theory(th.parent,th.name,th.meta,th.paramC,th.dfC)
      th.getDeclarations foreach {
        case c : FinalConstant if c.name.toString.contains("TCC") =>
        case o => nth add o
      }
      nth
    }
  }

  private val and = thpath ? "AND"
  private val not = thpath ? "NOT"
  private val or = thpath ? "OR"
  private val implies = thpath ? "IMPLIES"
  private val equiv = thpath ? "IFF"

  val preproc = (notccs + CovariantParameterPreprocessor + paramelim + LFHOASElim(vfhoas) + LFClassicHOLPreprocessor(
    proof.path,
    and,
    not,
    forall = Some(forall.path),
    exists = Some(exists.path),
    or = Some(or),
    implies = Some(implies),
    equiv = Some(equiv),
    equal = Some(equal.path)
  )).withKey("PVS").withKey(thpath)

  object parambind {
    val path: GlobalName = PVSTheory.rootdpath ? "BoundInclude" ? "parameter_binder"
    val term = OMS(path)
    def apply(sym : Term,pars : List[Term]) = sym match {
      case OMS(s) if pars.nonEmpty => OMA(this.term,OMS(s) :: pars)
      case _ => sym
    }
    def unapply(t : Term) : Option[(GlobalName,List[Term])] = t match {
      case OMA(this.term,OMS(sym) :: rest) => Some((sym,rest))
      case _ => None
    }
  }

   object tp extends sym("tp")
   //object unknown extends sym("unknown_type")

   object expr extends sym("expr") {
      def apply(t:Term) = Apply(this.term,t)
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(this.term,tm) => Some(tm)
         case _ => None
      }
   }

   object subtpissubtype extends sym("subtpissubtype") {
      def apply(tp1 : Term, tp2 : Term, proof : GlobalName) = ApplySpine(this.term,tp1,tp2,OMS(proof))
   }

   object nonempty extends sym("nonempty") {
      def apply(t:Term) = proof("internal_judgment",Apply(this.term,t))
      def tps = LFX.predsubtp(tp.term,Lambda(LocalName("x"),tp.term,apply(OMV("x"))))
   }
/*
   object is_nonempty extends sym("is_nonempty") {
      def apply(tp:Term,expr:Term) = ApplySpine(this.term,tp,expr)
   }
*/
   object bool extends sym("bool") {
      def unapply(t : Term) : Boolean = t match {
         case this.term => true
         case OMS(p) if p == PVSTheory.thpath ? "bool" => true
         case OMS(p) if p == PVSTheory.thpath ? "boolean" => true
         case _ => false
      }
   }

   object proof extends sym("proof") {
      def apply(kind:String,thm:Term) = {
         val t = Apply(this.term,thm)
         t.metadata.add(new MetaDatum(PVSTheory.thpath ? "formkind",new sym(kind).term))
         t
         // ApplySpine(this.term,new sym(kind).term,thm)
      }
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(this.term,a) => Some(a)
         case _ => None
      }
   }

   object forall extends sym("forall") {
      def apply(con:Context,tm:Term) = con.foldRight(tm)((v,t) => ApplySpine(this.term,v.tp.get,Lambda(v.name,expr(v.tp.get),t)))
   }

   object exists extends sym("exists") {
      def apply(con:Context,tm:Term) = con.foldRight(tm)((v,t) =>
         ApplySpine(this.term,v.tp.get,Lambda(v.name,expr(v.tp.get),t)))
   }

   object pvslambda extends sym("lambda") {
      def apply(x : LocalName,tp1:Term,tptarget:Term,body:Term) =
         ApplySpine(this.term,tp1,Lambda(x,expr(tp1),tptarget),Lambda(x,expr(tp1),body))
     /*
      def unapply(t:Term) : Option[(LocalName,Term,Term,Term)] = t match {
         case ApplySpine(this.term,List(tp1,Lambda(x1,expr(tp2),tptarget),Lambda(x2,expr(tp3),body))) =>
          Some((x1,tp1,tptarget,Lambda(x2,expr(tp3),body)))
      }
      */
     def unapply(t:Term) : Option[(LocalName,Term,Term,Term)] = t match {
       case ApplySpine(this.term,List(tp1,Lambda(x,expr(tp2),tptarget),Lambda(x2,expr(tp3),body))) =>
         Some((x,tp1,tptarget,body))
       case _ => None
     }
   }
  /*
  object lambdaspine {
    def apply(ls : List[(LocalName,Term)],tptarget : Term, body : Term) = GARBLGARBL
  }
*/

   object pvsapply extends sym("pvsapply") {
      def apply(f : Term,t:Term,ftp : Term)(state:TranslationState) : (Term,Term) = ftp match {
         case pvspi(x,tpx,rettp) => (ApplySpine(this.term,tpx,Lambda(x,expr(tpx),rettp),f,t),rettp ^? x/t) //Apply(tpf,t))
         case _ if state != null => (ApplySpine(this.term,state.doUnknown,state.doUnknown,f,t),state.doUnknown)
         case _ => throw new Exception("Unknown types in pvsapply")
      }
     def apply(f : Term, t: Term,tpA : Term, tpB : Term) =
       (ApplySpine(this.term,tpA,
         Lambda(LocalName("I")/"x",expr(tpA),tpB),f,t),tpB)

      def unapply(t : Term) : Option[(Term,Term,Term)] = t match {
         case ApplySpine(this.term,List(tpx,Lambda(y,btp,rettp),f,t2)) =>
          Some(f,t2,rettp ^? y/t2)
         case _ => None
      }
   }
  /*
  object pvsapplyspine {
    def apply(f : Term, args :)
  }
*/
   object fun_type extends sym("fun_type") {
      def apply(a:Term,b:Term) = ApplySpine(this.term,a,b)
      def unapply(t:Term) : Option[(Term,Term)] = t match {
         case ApplySpine(this.term,List(a,b)) => Some((a,b))
         case _ => None
      }
   }

   object pvspi extends sym("pvspi") {
      def apply(bound : LocalName, boundtp : Term, rettp : Term) = ApplySpine(this.term,boundtp,Lambda(bound,expr(boundtp),rettp))
      def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
         case ApplySpine(this.term,List(boundtp,Lambda(bound,expr(boundtp2),rettp))) =>
            Some(bound,boundtp,rettp)
         case fun_type(a,b) =>
            val x = Context.pickFresh(t.freeVars.map(VarDecl(_)),LocalName("__"))._1
            Some((x,a,b))
         case _ => None
      }
   }
  object pispine {
    def apply(ls : List[(LocalName,Term)],rettp : Term) : Term =
      if (ls.isEmpty) rettp else apply(ls.init,pvspi(ls.last._1,ls.last._2,rettp))
    def unapply(t : Term) : Option[(List[(LocalName,Term)],Term)] = t match {
      case pvspi(n,tp1,ret) =>
        val unap = unapply(ret)
        if (unap.isDefined) Some(((n,tp1) :: unap.get._1,unap.get._2))
        else Some((List((n,tp1)),ret))
      case _ => None
    }
  }

   object tuple_type extends sym("tuple_tp") {
      def apply(l:List[Term]) : Term = if (l.length==2) ApplySpine(this.term,l.head,l.tail.head) else
         if (l.length==1) l.head else
         ApplySpine(this.term,l.head,apply(l.tail))
      def unapply(t:Term) : Option[List[Term]] = t match {
         case ApplySpine(this.term,List(a,b)) => Some(a :: unapply(b).getOrElse(List(b)))
         case _ => None
      }
   }

   object pvssigma extends sym("pvssigma") {
      def apply(bound : LocalName, boundtp : Term, rettp : Term) = ApplySpine(this.term,boundtp,
         Lambda(bound,expr(boundtp),rettp))
      def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
         case ApplySpine(this.term,List(boundtp,Lambda(bound,boundtp2,rettp))) =>
            Some(bound,boundtp,rettp)
         case tuple_type(l) => Some((Context.pickFresh(t.freeVars.map(VarDecl(_)),LocalName("__"))._1,
           l.head,tuple_type(l.tail)))
         case _ => None
      }
   }
   object sigmaspine {
     def apply(ls : List[(LocalName,Term)],ret : Term) : Term =
       if (ls.isEmpty) ret
       else apply(ls.init,pvssigma(ls.last._1,ls.last._2,ret))
     def unapply(t : Term) : Option[(List[(LocalName,Term)],Term)] = t match {
       case pvssigma(n,tp1,ret) =>
         val unap = unapply(ret)
         Some(if (unap.isDefined) ((n,tp1) :: unap.get._1,unap.get._2) else (List((n,tp1)),ret))
       case _ => None
     }
   }

   object tuple_expr extends sym("tuple_expr") {
      def apply(l:List[(Term,Term)]) : (Term,Term) =
         if (l.length==2) (ApplySpine(this.term,l.head._2,l.tail.head._2,l.head._1,l.tail.head._1),
           tuple_type(List(l.head._2,l.tail.head._2)))
         else if (l.length == 1) l.head else {
            val ret = apply(l.tail)
            (ApplySpine(this.term,l.head._2,ret._2,l.head._1,ret._1),tuple_type(List(l.head._2,ret._2)))
         }
      def unapply(t : Term) : Option[List[(Term,Term)]] = t match {
         case ApplySpine(this.term,List(tA,tB,a,b)) => Some((a,tA) :: unapply(b).getOrElse(List((b,tB))))
         case _ => None
      }
   }
/*
   object typecast extends sym("TypeCast") {
      def apply(tpA:Term,tpB:Term,e: Term,prf:FinalConstant = null) =
         if (prf!=null) {
            val (kind,form) = prf.tp.get match {
               case proof(a,b) => (a,b)
               case _ =>
                  println("TCC is not a proof! " + prf.tp.get)
                  (new sym("internal_judgement").term,new sym("TRUE").term)
            }
            ApplySpine(this.term,tpA,tpB,kind,form,e,prf.toTerm)
         } else ApplySpine(this.term,tpA,tpB,new sym("internal_judgement").term,new sym("TRUE").term,e,
            new sym("proof_of_TRUE").term)
   }
   */

   object expr_as_type extends sym("expr_as_type") {
      def apply(expr : Term, tp : Term)(state : TranslationState) : Term = tp match {
        case pvspi(b,btp,PVSTheory.bool.term) => ApplySpine(this.term,tp,btp,expr)
        case _ =>
          ApplySpine(this.term,tp,state.doUnknown,expr)
          //setsub(state.doUnknown,expr)
          //println(expr)
          //println(tp)
          //sys.exit()
      }//ApplySpine(this.term,tp,expr)
   }

   object recursor extends sym("recursor") {
      def apply(tp:Term,name:LocalName,defi:Term) = ApplySpine(this.term,tp,Lambda(name,expr(tp),defi))
   }

   object selection extends sym("selection") {
      def apply(cons : Term, vars : Context, body : Term, constp : Term) =
         ApplySpine(this.term,constp,cons,if (vars.nonEmpty) OMBIND(new sym("selbind").term,
            vars.map(v => v.copy(tp = v.tp.map(expr(_)))),body) else body)
   }

   object pvsmatch extends sym("match") {
      def apply(tm : Term, tmtp : Term, cases : List[Term], rettp : Term) =
         ApplySpine(this.term, tmtp, rettp, tm, ApplySpine(new sym("caselist").term,cases:_*))
   }

   object subtpjudg extends sym("subtpjudg") {
      def apply(sub:Term,sup:Term) = ApplySpine(this.term,sub,sup)
   }

   object equal extends sym("=") {
      def apply(tp:Term,a:Term,b:Term)(state:ImportState) = pvsapply(
         Apply(this.term,tp),
         tuple_expr(List((a,tp),(b,tp)))._1,
         fun_type(tuple_type(List(tp,tp)),bool.term))(state)
   }

   object tpjudg extends sym("tpjudg") {
      def apply(tm : Term, tmtp : Term, restp : Term) = ApplySpine(this.term,tmtp,tm,restp)
   }

   object pred {
      val predpaths = List("pred","PRED","predicate","PREDICATE","setof","SETOF").map(s =>
         Path.parseS("http://pvs.csl.sri.com/Prelude?defined_types?"+s,NamespaceMap.empty))
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(OMS(p),a) if predpaths contains p => Some(a)
         case pvspi(_,a,bool.term) => Some(a)
         case _ => None
      }
   }

   object fieldapp extends sym("fieldapp") {
      def apply(tm : Term, field : String) = ApplySpine(this.term,tm,OML(LocalName(field),None,None))
   }

   object recordexpr extends sym("recordexpr") {
      def apply(nametpdf : (LocalName,Term,Term)*) = {
         val rtp = LFX.RecExp(OML(LocalName("DUMMY"),
           Some(expr(bool.term)),Some(OMS(PVSTheory.thpath ? "TRUE"))) :: nametpdf.map(t => OML(t._1, Some(tp.term), Some(t._2))).toList : _*)
         val rdf = LFX.RecExp(OML(LocalName("DUMMY"),
           Some(expr(bool.term)),Some(OMS(PVSTheory.thpath ? "TRUE"))) :: nametpdf.map(t => OML(t._1, Some(expr(t._2)), Some(t._3))).toList : _*)
         ApplySpine(this.term, rtp,rdf)
      }
   }

   object recordtp extends sym("rectp") {
      def apply(nametp : (LocalName,Term)*) =
         ApplySpine(this.term,LFX.RecExp(OML(LocalName("DUMMY"),
           Some(expr(bool.term)),Some(OMS(PVSTheory.thpath ? "TRUE"))) :: nametp.map(t => OML(t._1,Some(tp.term),Some(t._2))).toList :_*))
      def unapply(t:Term) : Option[List[(String,Term)]] = t match {
         case ApplySpine(this.term,List(LFX.RecExp(l))) => Some(l.fields.tail.map({
            case OML(name,Some(tp.term),Some(df),_,_) => (name.toString,df)
            case tm @ _ => throw new Exception("Invalid Record type element: " + tm)
         }))
         case _ => None
      }
   }

   object setsub extends sym("setsub") {
      def apply(tp:Term,expr:Term) = ApplySpine(this.term,tp,expr)
     def unapply(t : Term) : Option[(Term,Term)] = t match {
       case ApplySpine(this.term,List(tp,expr)) => Some((tp,expr))
       case _ => None
     }
   }

   object projection extends sym("proj") {
      def apply(tm:Term,tmtp:Term,i:Int)(state : TranslationState) = {
         val tp = tmtp match {
            case PVSTheory.tuple_type(l) if i<=l.length => l(i-1)
            case _ => state.doUnknown
         }
         (ApplySpine(this.term,tmtp,tp,tm,OMLIT(BigInt(i),NatLiterals)),tp)
      }
   }

   object enumtype extends sym("enumtype") {
      def apply(l:List[String]) = ApplySpine(this.term,l.map(StringLiterals(_)):_*)
   }

   object update extends sym("update") {
      def apply(e : Term, up : Term) = ApplySpine(this.term,List(e,up):_*)
   }

   object recupdate extends sym("recupdate") {
      def apply(field:String, expr:Term, args : List[Term] = Nil) =
         ApplySpine(this.term,OML(LocalName(field),None,None)::expr::args:_*)
   }

   object funupdate extends sym("funupdate") {
      def apply(arg:Term,value:Term) = ApplySpine(this.term,arg,value)
   }

   object tupleupdate extends sym("tupleupdate") {
      def apply(index:Int,value:Term) = ApplySpine(this.term,OMLIT(BigInt(index),NatLiterals),value)
   }

   object typeext extends sym("type_extension") {
      def apply(tp : Term, by : Term) = ApplySpine(this.term,tp,by)
   }

   object powertp extends sym("powertp") {
      def apply(tp : Term) = Apply(this.term,tp)
   }

   /*
   object subtpJudg extends sym("subtpJudg") {
      def apply(subtp : Term, suptp : Term) = ApplySpine(this.term,subtp,suptp)
   }

   // TODO temporary pseudosolution
   object tccs extends sym("tcclist") {
      def apply(tm:Term*) = if (tm.isEmpty) unknown.term else if (tm.length==1) tm.head else ApplySpine(this.term,tm:_*)
   }





   object constructor extends sym("constructor") {
      def apply(recognizer : String, accessors : List[(String,Term)]) = ApplySpine(this.term,
         OMLIT(recognizer,StringLiterals)::accessors.map(p =>OML(VarDecl(LocalName(p._1),Some(p._2),None,None))):_*)
   }
   */

}

object LFX {
  class LFRecSymbol(name:String) {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "Records") ? "Symbols" ? name
    val term = OMS(path)
  }
  class LFPredSubSymbol(name:String) {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "Subtyping") ? "PredSubSymbols" ? name
    val term = OMS(path)
  }
  class LFSubJudgSymbol(name:String) {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "Subtyping") ? "JudgmentSymbol" ? name
    val term = OMS(path)
  }

  case class RecordBody(self: Option[LocalName], fields: List[OML]) {
    /** names of all fields */
    def names = fields.map(_.name)
    /** checks for duplicate names */
    def hasDuplicates = utils.hasDuplicates(names)
    /** retrieve a field for a given name */
    def get(l: LocalName) = fields.find(_.name == l)
  }

  /** unifies record terms and types; the empty record is OMA(this.term,Nil), not this.term */
  class RecordLike(n: String) extends LFRecSymbol(n) {
    // there may not be an apply method that takes a context instead of a List[OML]
    def apply(v:OML*): Term = apply(None, v.toList)
    def apply(self: Option[LocalName], fields: List[OML]): Term = {
      self match {
        case None => OMA(this.term, fields)
        case Some(l) => OMBINDC(this.term, Context(VarDecl(l)), fields)
      }
    }
    def unapply(t : Term) : Option[RecordBody] = t match {
      case OMA(this.term, OMLList(fs)) => Some(RecordBody(None, fs))
      case OMBINDC(this.term, Context(VarDecl(n, None, None, None, _), rest@_*), OMLList(fs)) if rest.isEmpty => Some(RecordBody(Some(n), fs))
      case _ => None
    }
  }

  object RecExp extends RecordLike("Recexp")

  object predsubtp extends LFPredSubSymbol("predsubtp") {
    def apply(tp : Term, pred:Term) = OMA(this.term,List(tp,pred))
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(this.term,a :: b :: Nil) => Some((a,b))
      case _ => None
    }
  }

  object subtypeJudg extends LFSubJudgSymbol("subtypeJudge") {
    def apply(t1 : Term, t2 : Term) = OMA(term,List(t1,t2))
    def unapply(t : Term) : Option[(Term,Term)] = t match {
      case OMA(this.term,List(t1,t2)) => Some(t1,t2)
      case _ => None
    }
  }
}
