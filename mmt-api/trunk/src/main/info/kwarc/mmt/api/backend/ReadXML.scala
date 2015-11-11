package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import documents._
import metadata._
import modules._
import notations._
import symbols._
import objects._
import patterns._
import utils._
import ontology._
import presentation._

import scala.xml.{Node,NodeSeq,Utility}

/** A Reader parses XML/MMT and calls controller.add(e) on every found content element e
 *  
 *  The XML may contain CURIEs.
 *  However, all namespace prefixes must be declared on the toplevel omdoc element; other bindings are ignored.
 */
class XMLReader(val report: frontend.Report) extends frontend.Logger {
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
   private def addModule(m: Module, md: Option[MetaData], docOpt: Option[Document])(implicit cont: StructuralElement => Unit) {
      add(m, md)
      docOpt map (d => add(MRef(d.path, m.path, true)))
   }
   
   /**
    *  parses a document and forwards its declarations to the continuation function 
    *  @param dpath the URI of the document
    *  @param node the document
    *  
    *  If the document has a base attribute, it is used as the default namespace of modules.
    */
   def readDocument(dpath : DPath, node : Node)(implicit cont: StructuralElement => Unit) {
      val nsMap = NamespaceMap.fromXML(node)(dpath)
      node match {
        case <omdoc>{modules @ _*}</omdoc> =>
           val base = Path.parseD(xml.attr(node, "base"), nsMap)
           val nsMapB = nsMap(base)
           log("document with URI " + dpath + " found")
           val d = new Document(dpath, Nil, nsMapB)
           add(d)
           modules foreach {m => readIn(nsMapB, d, m)}
        case _ => throw ParseError("document expected: " + node)
      }
   }
   
   /** entry point for reading in a node if the containing document, theory, view is known */
   def readIn(nsMap: NamespaceMap, se: StructuralElement, node: Node)(implicit cont: StructuralElement => Unit) {
      // base is unchanged for documents, module.path for theories, codomain for views
      se match {
         case d: Document => readInDocument(nsMap, Some(d), node)
         case t: DeclaredTheory => readInModule(t.path, nsMap(t.path), Some(t), node)
         case v: DeclaredView => readInModule(v.path, nsMap(v.to.toMPath), Some(v), node)
         //case s: DeclaredStructure => readInTheory(s.home / s.name, s.path, Some(s), node)
      }
   }
   
   /**
    * reads a child of a document
    * @param nsMap used for relative paths, in particular for the namespace of modules
    * @param docOpt the containing document, if any; if given, XRef's will be generated
    * @param node the node to parse
    */
   def readInDocument(nsMap: NamespaceMap, docOpt: Option[Document], node : Node)(implicit cont: StructuralElement => Unit) {
      lazy val doc = docOpt.getOrElse {throw ParseError("document element without containing document")}
      node match {
         case <omdoc>{mods}</omdoc> =>
            val name = LocalName.parse(xml.attr(node,"name"),nsMap)
            val dpath = doc.path / name
            val innerdoc = new Document(dpath)
            add(DRef(doc.path, dpath, true))
            readIn(nsMap, innerdoc, mods)
         case <dref/> =>
	         val d = xml.attr(node, "target")
	         log("dref to " + d + " found")
	         val r = DRef(doc.path, Path.parseD(d,nsMap), false)
	         add(r)
         case <mref/> =>
	         val t = xml.attr(node, "target")
	         log("mref to " + t + " found")
	         val r = MRef(doc.path, Path.parseM(t,nsMap), false)
	         add(r)
         case scala.xml.Comment(_) =>
         case <metadata>{_*}</metadata> =>
            val md = MetaData.parse(node, nsMap)
            doc.metadata = md
         case node =>
           val (m, md) = MetaData.parseMetaDataChild(node, nsMap)
           val namespace = Path.parseD(xml.attr(m,"base"), nsMap)
           val name = LocalName.parse(xml.attr(m,"name"), nsMap(namespace))
           xml.trimOneLevel(m) match {
	         case <theory>{seq @ _*}</theory> =>
		         log("theory " + name + " found")
		         val tpath = namespace ? name
		         seq match {
		        	 case <definition>{d}</definition> =>
		        	   val df = Obj.parseTerm(d, nsMap(tpath))
		        	   val t = DefinedTheory(namespace, name, df)
		        	   addModule(t, md, docOpt)
		        	 case symbols => 
				         val meta = xml.attr(m, "meta") match {
				            case "" => None
				            case mt =>
				               log("meta-theory " + mt + " found")
				               Some(Path.parseM(mt, nsMap(namespace)))
				         }
				         val t = new DeclaredTheory(namespace, name, meta)
				         addModule(t, md, docOpt)
                     logGroup {
                        symbols.foreach {d => 
                           readIn(nsMap, t, d)
                        }
                     }
		         }
	         case <view>{_*}</view> =>
	            log("view " + name + " found")
	            val (m2, from) = getTheoryFromAttributeOrChild(m, "from", nsMap)
	            val (m3, to) = getTheoryFromAttributeOrChild(m2, "to", nsMap)
	            val isImplicit = parseImplicit(m)
	            m3.child match {
                  case <definition>{d}</definition> :: Nil =>
		               val df = Obj.parseTerm(d, nsMap)
		               val v = DefinedView(namespace, name, from, to, df, isImplicit)
		               addModule(v, md, docOpt)
                  case assignments =>
	 		            val v = new DeclaredView(namespace, name, from, to, isImplicit) // TODO add metamorph?
                     addModule(v, md, docOpt)
                     logGroup {
                        assignments.foreach {d =>
                           readIn(nsMap, v, d)
                        }
                     }
                }
	         case <rel>{_*}</rel> => 
	            //ignoring logical relations, produced by Twelf, but not implemented yet
	         /*
	         case (base : DPath, <style>{notations @ _*}</style>) =>
		         log("style " + name + " found")
			      val npath = base ? name
		         val from = Path.parse(xml.attr(m,"from"), base)
               val to = Path.parse(xml.attr(m, "to"), utils.mmt.mimeBase)
		         val nset = new Style(base, name, from, to)
		         add(nset, md)
		         docParent map (dp => add(MRef(dp, npath, true)))
		         readNotations(npath, from, notations)
		         */
           case n if Utility.trimProper(n).isEmpty => //whitespace node => nothing to do 
	         case _ => throw ParseError("module level element expected: " + m)
         }
      }
   }
   
   /**
    * @param home the mpath to use to refer to module (module.path for theories and views)
    * @param nsMap used for relative paths (base is set by readIn)
    * @param module the containing theory, view, or structure (of type Body)
    * @param node the node to parse 
    */
   def readInModule(home: MPath, nsMap: NamespaceMap, moduleOpt: Option[StructuralElement], node: Node)(implicit cont: StructuralElement => Unit) {
      val homeTerm = OMMOD(home)
      def doPat(name : LocalName, parOpt : Option[Node], con : Node, xmlNotation : NodeSeq, md: Option[MetaData]) {
    	  log("pattern " + name.toString + " found")
    	  val pr = parOpt match {
    	 	  case Some(par) => Context.parse(par, nsMap)
    	 	  case None      => Context()
    	  }
    	  val cn = Context.parse(con, nsMap)
        val notation = NotationContainer.parse(xmlNotation, home ? name)
    	  val p = new Pattern(homeTerm, name, pr, cn, notation)
    	  add(p, md)
      }
      val name = LocalName.parse(xml.attr(node,"name"), nsMap)
      val alias = xml.attr(node, "alias") match {
         case "" => None
         case a => Some(LocalName.parse(a))
      }
      val (symbol, md) = MetaData.parseMetaDataChild(node, nsMap)
      lazy val module = moduleOpt.getOrElse {
         throw ParseError("missing containing module")
      } 
      xml.trimOneLevel(symbol) match {
         case <constant>{comps @_*}</constant> =>
            log("constant " + name.toString + " found")
            var tp: Option[Term] = None
            var df: Option[Term] = None
            var notC: Option[NotationContainer] = None
            
            comps.map(xml.trimOneLevel) foreach {
               case <type>{t}</type> => tp match {
                  case None =>
                     tp = Some(Obj.parseTerm(t, nsMap))
                  case Some(_) =>
                     throw ParseError("multiple types in " + symbol)
               }
               case <definition>{d}</definition> => df match {
                  case None =>
                     df = Some(Obj.parseTerm(d, nsMap))
                  case Some(_) =>
                     throw ParseError("multiple definitions in " + symbol)
               }
               case comp @ (<notations>{_*}</notations>) => notC match {
                  case None =>
                     notC = Some(NotationContainer.parse(comp.child, home ? name))
                  case Some(_) =>
                     throw ParseError("multiple notation children in " + symbol)
               }
               case c => throw ParseError("illegal child in constant " + c)
            }
            val rl = xml.attr(symbol,"role") match {
               case "" => None
               case r => Some(r)
            }
            val c = Constant(homeTerm, name, alias, tp, df, rl, notC.getOrElse(NotationContainer()))
            add(c,md)
         case imp @ <import>{seq @ _*}</import> =>
            log("import " + name + " found")
            val (rest, from) = getTheoryFromAttributeOrChild(imp, "from", nsMap)
            val adjustedName = if (name.length > 0) name else from match {
               case OMMOD(p) => LocalName(p)
               case _ => throw ParseError("domain of include must be atomic")
            }
            val isImplicit = parseImplicit(symbol) 
            rest.child.map(xml.trimOneLevel) match {
               case <definition>{d}</definition> :: Nil =>
                  val df = Obj.parseTerm(d, nsMap)
                  val s = DefinedStructure(homeTerm, adjustedName, from, df, isImplicit)
                  add(s,md)
               case assignments =>
                  val s = DeclaredStructure(homeTerm, adjustedName, from, isImplicit)
                  add(s,md)
                  assignments foreach {a => readInModule(home / adjustedName, nsMap, Some(s), a)}
            }
         case <theory>{body @_*}</theory> =>
            val parent = home.parent
            val tname = home.name / name
            body.map(xml.trimOneLevel) match {
               case <definition>{d}</definition> =>
                  val df = Obj.parseTerm(d, nsMap)
                  val t = DefinedTheory(parent, tname, df)
                  add(new NestedModule(t), md)
               case symbols =>
                  val meta = xml.attr(symbol, "meta") match {
                     case "" => None
                     case mt =>
                        log("meta-theory " + mt + " found")
                        Some(Path.parseM(mt, nsMap))
                  }
                  val t = new DeclaredTheory(parent, tname, meta)
                  add(new NestedModule(t), md)
                  symbols.foreach {d => 
                     logGroup {
                        readIn(nsMap, t, d)
                     }
                  }
            }
         case <ruleconstant/> =>
            log("found rule constant " + name + ", trying RuleConstantInterpreter")
            val rc = RuleConstantInterpreter.fromNode(symbol, home)
            add(rc, md)
         case <parameters>{parN}</parameters> =>
            val par = Context.parse(parN, nsMap)
            module match {
               case d: DeclaredTheory => d.parameters = par
               case _ => throw ParseError("parameters outside declared theory")
            }
         case <metadata>{_*}</metadata> =>
            val md = MetaData.parse(node, nsMap)
            module.metadata = md
         //TODO remove patterns and instances
         case <pattern>{ch @_*}</pattern> => 
           log("pattern with name " + name + " found")
           <pattern> {ch.map(xml.trimOneLevel)} </pattern> match {
             case <pattern><parameters>{params}</parameters><declarations>{decls}</declarations></pattern> =>
                log("pattern with name " + name + " found")
                doPat(name, Some(params), decls, Nil, md)
             case <pattern><parameters>{params}</parameters><declarations>{decls}</declarations><notation>{ns @_*}</notation></pattern> =>
                log("pattern with name " + name + " found")
                doPat(name, Some(params), decls, ns, md)
             case <pattern><declarations>{decls}</declarations></pattern> =>
                log("pattern with name " + name + " found")
                doPat(name, None, decls, Nil, md)         
             case <pattern><declarations>{decls}</declarations><notation>{ns @ _*}</notation></pattern> =>
                log("pattern with name " + name + " found")
                doPat(name, None, decls, ns, md)
           }
         case <instance>{ns @ _*}</instance> =>
            val p = xml.attr(symbol,"pattern")
         	log("instance " + name.toString + " of pattern " + p + " found")
         	val args = ns map (Obj.parseTerm(_, nsMap))
            val inst = new Instance(homeTerm,name,Path.parseS(p,nsMap),args.toList)
            add(inst, md)
         case scala.xml.Comment(_) =>
         case n if Utility.trimProper(n).isEmpty => //whitespace node => nothing to do 
         case _ => throw ParseError("symbol level element expected: " + symbol)
      }
   }
   /*
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
   */
   private def parseImplicit(n: Node): Boolean = {
      xml.attr(n, "implicit") match {
         case "true" => true
         case "false" | "" => xml.attr(n, "name") == ""
         case s => throw ParseError("true|false expected in implicit attribute, found " + s)
      }
   }
   /** parses a theory using the attribute or child "component" of "n", returns the remaining node and the theory */
   private def getTheoryFromAttributeOrChild(n: Node, component: String, nsMap: NamespaceMap) : (Node, Term) = {
      if (n.attribute(component).isDefined) {
         (n, OMMOD(Path.parseM(xml.attr(n, component), nsMap)))
      } else {
          val (newnode, tOpt) = splitOffChild(n, component)
          tOpt match {
             case Some(t) =>
                val tT = xml.trimOneLevel(t)
                if (tT.child.length == 1) (newnode, Obj.parseTerm(tT.child(0), nsMap))
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
