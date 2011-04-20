package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation._
import scala.xml.{Node,NodeSeq}

/** A Reader parses XML/MMT and calls controller.add(e) on every found content element e */
class Reader(controller : frontend.Controller, report : frontend.Report) {
   def log(s : String) = report("reader", s)
   def add(e : StructuralElement) {
      controller.add(e)
   }
   
   private def splitOffMetaData(node : Node) : (Node, Option[Node]) = {
      node match {
        case scala.xml.Elem(p,l,a,s,cs) =>
           var md : Option[Node] = None
           val cs2 = cs flatMap {
              case e @ <metadata>{_*}</metadata> => md = Some(e); Nil 
              case e => e
           }
           (scala.xml.Elem(p,l,a,s,cs2 : _*), md)
        case n => (n, None)
      }
   }
   
   def read(p : DPath, node : Node, eager : Boolean) = node.label match {
      case "omdoc" => readDocuments(p, node)
      case "assertions" => readAssertions(node)
      case l => throw ParseError("unexpected label: " + l)
   }
   
   def readDocuments(location : DPath, documents : NodeSeq) {
      documents foreach {readDocument(location, _)}
   }
   def readDocument(location : DPath, D : Node) : DPath = {
        D match {
        case <omdoc>{modules @ _*}</omdoc> =>
           val path = xml.attr(D, "base") match {case "" => location case s => DPath(new xml.URI(s))}
           log("document with base " + path + " found")
           val d = new Document(path)
           add(d)
           readModules(path, Some(path), modules)
           path
        case <mmtabox>{decls @ _*}</mmtabox> =>
           readAssertions(decls)
           location
        case _ => throw new ParseError("document expected: " + D)
      }
   }
   
   /** calls docParent.get if on document elements */
   def readModules(modParent : DPath, docParent : Option[DPath], modules : NodeSeq) {
      for (m <- modules) {
         m match {
         case <dref/> =>
	         val d = xml.attr(m, "target")
	         log("dref to " + d + " found")
	         val r = DRef(docParent.get, Path.parseD(d,modParent), false)
	         add(r)
         case <mref/> =>
	         val t = xml.attr(m, "target")
	         log("mref to " + t + " found")
	         val r = MRef(docParent.get, Path.parseM(t,modParent), false)
	         add(r)
         case scala.xml.Comment(_) =>
         case <metadata>{_*}</metadata> => 
         case _ =>
           val name = Path.parseName(xml.attr(m,"name")).toLocalPath
           val base = Path.parse(xml.attr(m,"base"), modParent)
           (base, m) match {
	         case (base : DPath, <theory>{seq @ _*}</theory>) =>
		         log("theory " + name + " found")
		         val tpath = base ? name
		         val (t, body) = seq match {
		        	 case <definition>{d}</definition> =>
		        	   val df = Obj.parseTheory(d, tpath)
		        	   (new DefinedTheory(modParent, name, df), None)
		        	 case symbols => 
				         val meta = xml.attr(m, "meta") match {
				            case "" => None
				            case mt =>
				               log("meta-theory " + mt + " found")
				               Some(Path.parseM(mt, base))
				         }
				         (new DeclaredTheory(base, name, meta), Some(symbols))
		         }
        	     add(t)
        	     docParent map (dp => add(MRef(dp, tpath, true)))
              body.foreach {d => 
        	              report.indent
                       readSymbols(tpath, tpath, d)
        	              report.unindent
        	     }
	         case (base : DPath, <view>{seq @ _*}</view>) =>
	            log("view " + name + " found")
	            val vpath = base ? name
	            val from = OMMOD(Path.parseM(xml.attr(m, "from"), base))
	            val to = OMMOD(Path.parseM(xml.attr(m, "to"), base))
	            val (v, body) = seq match {
                  case <definition>{d}</definition> =>
		            val df = Obj.parseMorphism(d, vpath)
		            (new DefinedView(modParent, name, from, to, df), None)
	              case assignments =>
	 		        (new DeclaredView(base, name, from, to), Some(assignments))
                }
	            add(v)
	            docParent map (dp => add(MRef(dp, vpath, true)))
			      body.foreach {d =>
	               report.indent
	               readAssignments(OMMOD(vpath), to.toMPath, d) //TODO relative names will be resolved wrong
	               report.unindent
	            }
	         case (base : DPath, <style>{notations @ _*}</style>) =>
		         log("style " + name + " found")
			     val npath = base ? name
		         val from = Path.parse(xml.attr(m,"from"), base)
		         val defaults = Defaults.parse(xml.attr(m, "defaults", "use"))
                 val to = Path.parse(xml.attr(m, "to"), utils.mmt.mimeBase)
		         val nset = new Style(base, name, defaults, from, to, report)
		         add(nset)
		         docParent map (dp => add(MRef(dp, npath, true)))
		         readNotations(npath, from, notations)
             case (base : DPath, <omdoc>{mods}</omdoc>) =>
                 val dpath = docParent.get / name
                 val doc = new Document(dpath)
                 add(DRef(docParent.get, dpath, true))
                 readModules(base, Some(dpath), mods)
             case (base : MPath, <notation>{_*}</notation>) =>
                 readNotations(base, base, m)
	         case (_,_) => throw new ParseError("module level element expected: " + m)
         }}
      }
   }
   def readSymbols(tpath : MPath, base: Path, symbols : NodeSeq) {
      val thy = OMMOD(tpath)
      def doCon(name : LocalName, t : Option[Node], d : Option[Node], r : String) {
         log("constant " + name + " found")
         val uv = Universe.parse(r)
         val tp = t.map(Obj.parseTerm(_, base))
         val df = d.map(Obj.parseTerm(_, base))
         val c = new Constant(thy, name, tp, df, uv)
         add(c)
      }
      for (s <- symbols; name = Path.parseName(xml.attr(s,"name")).toLocalName) {
         val (s2, md) = splitOffMetaData(s) 
         s2 match {
         case <constant><type>{t}</type><definition>{d}</definition></constant> =>
            doCon(name,Some(t),Some(d),xml.attr(s,"role"))
         case <constant><definition>{d}</definition><type>{t}</type></constant> =>
            doCon(name,Some(t),Some(d),xml.attr(s,"role"))
         case <constant><type>{t}</type></constant> =>
            doCon(name,Some(t),None,xml.attr(s,"role"))
         case <constant><definition>{d}</definition></constant> =>
            doCon(name,None,Some(d), xml.attr(s,"role"))
         case <constant/> =>
            doCon(name,None,None,xml.attr(s,"role"))
         case <structure>{seq @ _*}</structure> =>
            log("structure " + name + " found")
            val from = OMMOD(Path.parseM(xml.attr(s, "from"), base))
            seq match {
               case <definition>{d}</definition> =>
                  val df = Obj.parseMorphism(d, base)
                  val i = new DefinedStructure(thy, name, from, df)
                  add(i)
               case assignments =>
                  val i = new DeclaredStructure(thy, name, from)
                  add(i)
                  readAssignments(OMDL(thy, name), base, assignments)
            }
         case <alias/> =>
            val forpath = Path.parseS(xml.attr(s, "for"), base)
            log("found alias " + name + " for " + forpath)
            add(new Alias(thy, name, forpath))
         case <include/> =>
            val from = Path.parseM(xml.attr(s, "from"), base)
            log("include from " + from + " found")
            add(PlainInclude(from, tpath))
         case <notation>{_*}</notation> =>
            readNotations(tpath, base, s)
         case <pattern><type>{tp}</type></pattern> =>
            log("pattern with name " + name + " found")
            val p = new Pattern(thy, name, Some(Obj.parseTerm(tp,base)), None)
            add(p)
         case <instance>{ms @ _*}</instance> =>
             val lm = ms.toList map {
                case m @ <match/> =>
                   val a = xml.attr(m,"value")
                   val n = try {a.toInt}
                           catch {case _ => throw ParseError("value must be natural number: " + a)}
                   if (n < 0) throw ParseError("value must be natural number: " + n)
                   Length(n)
                case <match>{ls @ _*}</match> => OMOBJ(ls.toList map {l => Obj.parseTerm(l,base)})
                }
            val inst = new Instance(thy,name,Path.parseS(xml.attr(s2,"pattern"),base),lm)
            add(inst)
         case scala.xml.Comment(_) =>
         case _ => throw new ParseError("symbol level element expected: " + s)
         }
      }
   }
   def readAssignments(link : Morph, base : Path, assignments : NodeSeq) {
      for (A <- assignments) {
         val name = Path.parseLocal(xml.attr(A, "name")).toLocalName
         A match {
            case <conass>{t}</conass> =>
               log("assignment for " + name + " found")
               val tg = Obj.parseTerm(t, base)
               val m = new ConstantAssignment(link, name, tg)
               add(m)
            case <strass>{t}</strass> =>
               log("assignment for " + name + " found")
               val tg = Obj.parseMorphism(t, base)
               val m = new DefLinkAssignment(link, name, tg)
               add(m)
            case <include>{mor}</include> =>
               val of = Obj.parseMorphism(mor, base)
               log("include of " + of + " found")
               val f = xml.attr(A, "from")
               val from = if (f == "")
                  Morph.domain(of)(controller.library) // throws Invalid if domain cannot be inferred
               else
                  OMMOD(Path.parseM(f, base))
               add(new DefLinkAssignment(link, LocalName(IncludeStep(from)), of))
            case <open/> =>
               log("open for " + name + " found")
               val as = xml.attr(A, "as") match {case "" => None case a => Some(a)}
               val m = new Open(link, name, as)
               add(m)
            case scala.xml.Comment(_) =>
            case _ => throw ParseError("assignment expected: " + A)
         }
      }
   }
   def readNotations(nset : MPath, base : Path, notations : NodeSeq) {
      for (N <- notations) {
         N match {
	        case <notation>{_*}</notation> =>
	           val forpath = xml.attr(N,"for") match {
	              case "" => None
                  case s => Some(Path.parse(s, base))
               }
               val roles : List[String] = xml.attr(N,"role").split("\\s").toList
               roles.map {case r =>
                  val role = info.kwarc.mmt.api.Role.parse(r)
	              val key = NotationKey(forpath, role)
	              log("notation read for " + key)
                  val not = Notation.parse(N, nset, key)
                  add(not)
               }
	        case <include/> =>
	           val from = Path.parseM(xml.attr(N, "from"), base)
               log("notation import found from " + from)
	           add(NotationImport(from, nset))
            case scala.xml.Comment(_) =>
            case _ => throw ParseError("notation expected: " + N)
	     }
	  }
   }
   def readAssertions(assertions : NodeSeq) {
      val deps = controller.depstore
      for (ass <- assertions) {
         log("assertion found: " + ass.toString)
         ass match {
	         case <individual/> =>
	            var pred = xml.attr(ass, "predicate")
                deps += ontology.Individual(Path.parse(xml.attr(ass, "path"), mmt.mmtbase), Unary.parse(pred))
	         case <relation/> =>
	           val subj = Path.parse(xml.attr(ass, "subject"), mmt.mmtbase)
               val obj  = Path.parse(xml.attr(ass, "object"), mmt.mmtbase)
	           val pred = Binary.parse(xml.attr(ass, "predicate"))
	           deps += Relation(pred, subj, obj)
	         case scala.xml.Comment(_) =>
	         case  _ => throw ParseError("ABox assertion expected: " + ass)
         }
      }
   }
}
