package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import documents._
import metadata._
import modules._
import parser.TextNotation
import symbols._
import objects._
import patterns._
import utils._
import ontology._
import presentation._

import scala.xml.{Node,NodeSeq}

/** A Reader parses XML/MMT and calls controller.add(e) on every found content element e */
class XMLReader(controller : frontend.Controller) extends Reader(controller) {
   val logPrefix = "reader"
   /** calls the continuation function */
   private def add(e : StructuralElement)(implicit cont: StructuralElement => Unit) {
      cont(e)
   }
   /** adds metadata and calls the continuation functions */
   private def add(e : StructuralElement, md: Option[MetaData])(implicit cont: StructuralElement => Unit) {
      md map {e.metadata = _}
      cont(e)
   }
   
   def read(p : DPath, node : Node, eager : Boolean)(implicit cont: StructuralElement => Unit) = node.label match {
      case "omdoc" => readDocuments(p, node)
      case "assertions" => readAssertions(node)
      case l => throw ParseError("unexpected label: " + l)
   }
   /** parses a sequence of documents (xml.Node) into the controller */
   def readDocuments(location : DPath, documents : NodeSeq)(implicit cont: StructuralElement => Unit) {
      documents foreach {readDocument(location, _)}
   }
   /** parses a document (xml.Node) and forwards its declarations into the continuation function */
   def readDocument(location : DPath, D : Node)(implicit cont: StructuralElement => Unit) {
      D match {
        case <omdoc>{modules @ _*}</omdoc> =>
           val path = Path.parseD(xml.attr(D, "base"), location)
           log("document with base " + path + " found")
           val d = new Document(location)
           add(d)
           readModules(path, Some(location), modules)
        case <mmtabox>{decls @ _*}</mmtabox> =>
           readAssertions(decls)
        case _ => throw ParseError("document expected: " + D)
      }
   }
   
   /** calls docParent.get if on document elements */
   def readModules(modParent : DPath, docParent : Option[DPath], modules : NodeSeq)(implicit cont: StructuralElement => Unit) {
      for (modmd <- modules) {
         val (m, md) = MetaData.parseMetaDataChild(modmd, modParent)
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
         case <metadata>{_*}</metadata> => //TODO
         case _ =>
           val name = Path.parseName(xml.attr(m,"name")).toLocalPath
           val base = Path.parse(xml.attr(m,"base"), modParent)
           (base, m) match {
	         case (base : DPath, <theory>{seq @ _*}</theory>) =>
		         log("theory " + name + " found")
		         val tpath = base ? name
		         val (t, body) = seq match {
		        	 case <definition>{d}</definition> =>
		        	   val df = Obj.parseTerm(d, tpath)
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
        	     add(t, md)
        	     docParent map (dp => add(MRef(dp, tpath, true)))
              body.foreach {d => 
        	              report.indent
                       readSymbols(tpath, tpath, d)
        	              report.unindent
        	     }
	         case (base : DPath, <view>{_*}</view>) =>
	            log("view " + name + " found")
	            val vpath = base ? name
	            val (m2, from) = XMLReader.getTheoryFromAttributeOrChild(m, "from", base)
	            val (m3, to) = XMLReader.getTheoryFromAttributeOrChild(m2, "to", base)
	            val isImplicit = parseImplicit(m)
	            val (v, body) = m3.child match {
                  case <definition>{d}</definition> :: Nil =>
		               val df = Obj.parseTerm(d, vpath)
		               (new DefinedView(modParent, name, from, to, df, isImplicit), None)
                  case assignments =>
	 		            (new DeclaredView(base, name, from, to, isImplicit), Some(assignments))
                }
	            add(v, md)
	            docParent map (dp => add(MRef(dp, vpath, true)))
			      body.foreach {d =>
	               report.indent
	               readAssignments(OMMOD(vpath), to.toMPath, d) //TODO relative names will be resolved wrong
	               report.unindent
	            }
	         case (_, <rel>{_*}</rel>) => Unit //ignoring logical relations, produced by Twelf, but not implemented yet
	         case (base : DPath, <style>{notations @ _*}</style>) =>
		         log("style " + name + " found")
			      val npath = base ? name
		         val from = Path.parse(xml.attr(m,"from"), base)
               val to = Path.parse(xml.attr(m, "to"), utils.mmt.mimeBase)
		         val nset = new Style(base, name, from, to)
		         add(nset, md)
		         docParent map (dp => add(MRef(dp, npath, true)))
		         readNotations(npath, from, notations)
             case (base : DPath, <omdoc>{mods}</omdoc>) =>
                 val dpath = docParent.get / name
                 val doc = new Document(dpath)
                 add(DRef(docParent.get, dpath, true), md)
                 readModules(base, Some(dpath), mods)
             case (base : MPath, <notation>{_*}</notation>) =>
                 readNotations(base, base, m)
	         case (_,_) => throw ParseError("module level element expected: " + m)
         }}
      }
   }
   def readSymbols(tpath : MPath, base: Path, symbols : NodeSeq)(implicit cont: StructuralElement => Unit) {
      val thy = OMMOD(tpath)
      def doPat(name : LocalName, parOpt : Option[Node], con : Node, xmlNotation : Option[Node], md: Option[MetaData]) {
    	  log("pattern " + name.toString + " found")
    	  val pr = parOpt match {
    	 	  case Some(par) => Context.parse(par, base)
    	 	  case None      => Context()
    	  }
    	  val cn = Context.parse(con, base)
        val notation = xmlNotation.map(TextNotation.parse(_, tpath ? name))
    	  val p = new Pattern(thy, name, pr, cn, notation)
    	  add(p, md)
      }
      for (s <- symbols) {
         val name = LocalName.parse(xml.attr(s,"name"))
         val alias = xml.attr(s, "alias") match {
            case "" => None
            case a => Some(LocalName.parse(a))
         }
         val (s2, md) = MetaData.parseMetaDataChild(s, base) 
         def doCon(t : Option[Node], d : Option[Node], xmlNotation : Option[Node]) {
            log("constant " + name.toString + " found")
            val tp = t.map(Obj.parseTerm(_, base))
            val df = d.map(Obj.parseTerm(_, base))
            val notation = xmlNotation.map(TextNotation.parse(_, tpath ? name))
            val rl = xml.attr(s,"role") match {
               case "" => None
               case r => Some(r)
            }
            val c = Constant(thy, name, alias, tp, df, rl, notation)  //TODO parse <notation>
            add(c,md)
         }
         s2 match {
         case <constant><type>{t}</type><definition>{d}</definition><notation>{n}</notation></constant> =>
            doCon(Some(t),Some(d), Some(n))
         case <constant><type>{t}</type><definition>{d}</definition></constant> =>
            doCon(Some(t),Some(d), None)
         case <constant><definition>{d}</definition><type>{t}</type></constant> =>
            doCon(Some(t),Some(d), None)
         case <constant><type>{t}</type></constant> =>
            doCon(Some(t),None, None)
         case <constant><type>{t}</type><notation>{n}</notation></constant> =>
           doCon(Some(t),None, Some(n))
         case <constant><definition>{d}</definition></constant> =>
            doCon(None,Some(d), None)
         case <constant><definition>{d}</definition><notation>{n}</notation></constant> =>
            doCon(None,Some(d), Some(n))
         case <constant><notation>{n}</notation></constant> =>
            doCon(None, None, Some(n))
         case <constant/> =>
            doCon(None,None,None)
         case <import>{seq @ _*}</import> =>
            log("import " + name + " found")
            val (rest, from) = XMLReader.getTheoryFromAttributeOrChild(s2, "from", base)
            val fromPath = from match {
               case OMMOD(p) => p
               case _ => throw ParseError("domain of structure must be atomic")
            }
            val isImplicit = parseImplicit(s2) 
            rest.child match {
               case <definition>{d}</definition> :: Nil =>
                  val df = Obj.parseTerm(d, base)
                  val s = new DefinedStructure(thy, name, fromPath, df, isImplicit)
                  add(s,md)
               case assignments =>
                  val s = new DeclaredStructure(thy, name, fromPath, isImplicit)
                  add(s,md)
                  readAssignments(OMDL(thy, name), base, assignments)
            }
         case <alias/> =>
            //TODO: remove this case when Twelf exports correctly
            log("warning: ignoring deprecated alias declaration")
         case <pattern><parameters>{params}</parameters><declarations>{decls}</declarations></pattern> =>
            log("pattern with name " + name + " found")
            doPat(name, Some(params), decls, None, md)
         case <pattern><parameters>{params}</parameters><declarations>{decls}</declarations><notation>{not}</notation></pattern> =>
            log("pattern with name " + name + " found")
            doPat(name, Some(params), decls, Some(not), md)
         case <pattern><declarations>{decls}</declarations></pattern> =>
            log("pattern with name " + name + " found")
            doPat(name, None, decls, None, md)         
         case <pattern><declarations>{decls}</declarations><notation>{not}</notation></pattern> =>
            log("pattern with name " + name + " found")
            doPat(name, None, decls, Some(not), md)         
         case <instance>{ns @ _*}</instance> =>
            val p = xml.attr(s2,"pattern")
         	log("instance " + name.toString + " of pattern " + p + " found")
         	val args = ns map (Obj.parseTerm(_, base))
            val inst = new Instance(thy,name,Path.parseS(p,base),args.toList)
            add(inst, md)
         case scala.xml.Comment(_) =>
         case _ => throw new ParseError("symbol level element expected: " + s2)
         }
      }
   }
   def readAssignments(link : Term, base : Path, assignments : NodeSeq)(implicit cont: StructuralElement => Unit) {
      for (amd <- assignments) {
         val (a, md) = MetaData.parseMetaDataChild(amd, base) 
         val name = LocalName.parse(base, xml.attr(a, "name"))
         val alias = xml.attr(amd, "alias") match {
            case "" => None
            case al => Some(LocalName.parse(al))
         }
         a match {
            case <constant>{t}</constant> =>
               log("assignment for " + name + " found")
               val tg = Obj.parseTerm(t, base)
               val m = new ConstantAssignment(link, name, alias, Some(tg))
               add(m, md)
            case <import>{_*}</import> =>
               log("assignment for " + name + " found")
               val (rest, from) = XMLReader.getTheoryFromAttributeOrChild(a, "domain", base)
               val fromPath = from match {
                  case OMMOD(p) => p
                  case _ => throw ParseError("domain of imported morphism must be atomic")
               }
               rest.child match {
                  case <value>{t}</value> :: Nil =>
                     val tg = Obj.parseTerm(t, base)
                     val m = new DefLinkAssignment(link, name, fromPath, tg)
                     add(m, md)
                  case c => throw ParseError("value expected: " + c)
               }
            case <open/> =>
               //TODO: remove this case when Twelf exports correctly
               log("warning: ignoring deprecated 'open' declaration")
            case scala.xml.Comment(_) =>
            case _ => throw ParseError("assignment expected: " + a)
         }
      }
   }
   def readNotations(nset : MPath, base : Path, notations : NodeSeq)(implicit cont: StructuralElement => Unit) {
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
                  val not = presentation.StyleNotation.parse(N, nset, key)
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
   def readAssertions(assertions : NodeSeq)(implicit cont: StructuralElement => Unit) {
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
   
   private def parseImplicit(n: Node): Boolean = {
      xml.attr(n, "implicit") match {
         case "true" => true
         case "false" | "" => xml.attr(n, "name") == ""
         case s => throw ParseError("true|false expected in implicit attribute, found " + s)
      }
   }
}

object XMLReader {
   /** parses a theory using the attribute or child "component" of "n", returns the remaining node and the theory */
   private def getTheoryFromAttributeOrChild(n: Node, component: String, base: Path) : (Node, Term) = {
      if (n.attribute(component).isDefined) {
         (n, OMMOD(Path.parseM(xml.attr(n, component), base)))
      } else {
          val (newnode, tOpt) = splitOffChild(n, component)
          tOpt match {
             case Some(t) =>
                if (t.child.length == 1) (newnode, Obj.parseTerm(t.child(0), base))
                else throw ParseError("ill-formed theory: " + t)
             case _ => throw ParseError("no component " + component + " found: " + n)
         }
      }
   }
   /** removes the child with label "label" from "node" (if any), returns the remaining node and that child */
   private def splitOffChild(node: Node, label : String) : (Node, Option[Node]) = node match {
       case scala.xml.Elem(p,l,a,s,cs @ _*) =>
           var n : Option[Node] = None
           val cs2 = cs flatMap {e =>
              if (e.label == label) {
                 n = Some(e)
                 Nil
              } else
                 e
           }
           (scala.xml.Elem(p,l,a,s,true,cs2 : _*), n)
       case n => (n, None)
   }
}
