package info.kwarc.mmt.coq

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Dim, RedirectableDimension}
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule, PlainInclude}
import info.kwarc.mmt.api.utils.{File, URI, xml}
import info.kwarc.mmt.coq.coqxml.CoqEntry

import scala.collection.mutable
import scala.xml.{Node, parsing}
import scala.xml.parsing.XhtmlParser

class Importer extends archives.Importer {
  val key = "coq-omdoc"


  def inExts = List("theory.xml")

  override def inDim = RedirectableDimension("Coq", Some(Dim("Coq")))

  // private val parseXML = syntax.makeParser

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    log("Reading " + bf.inFile)
    val es = try {
      val parser = new Parser(bf.inFile)
      parser.parse
    } catch {
      case e: utils.ExtractError =>
        log(e.getMessage)
        sys.exit
    }

    index(doDocument(bf.narrationDPath.^,bf.inFile.name.split('.').head,es))

    BuildResult.empty
  }

  def doDocument(dp : DPath,file : String,elems : List[coqxml.theorystructure]): Document = {
    val d = new Document(dp)
    val top = Theory(d.path,LocalName(file),Some(Coq.foundation))
    controller add top
    elems.foreach(doDecl(top,_))
    controller add MRef(d.path,top.path)
    d
  }

  def doDecl(parent : Theory, elem : coqxml.theorystructure): Unit = elem match {
    case coqxml.MODULE(uri,as,comp) =>
      val name = parent.name / uri.path.last
      val th = Theory(parent.parent,name,Some(Coq.foundation))
      val nt = new NestedModule(OMMOD(parent.path),LocalName(uri.path.last),th)
      th.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
      log("Module " + th.path.toString + " ~> " + nt.path.toString)
      controller add nt
      comp.foreach(doDecl(th,_))
      controller add PlainInclude(th.path,parent.path)
    case coqxml.SECTION(uri,statements) =>
      val name = parent.name / uri.path.last
      val th = Theory(parent.parent,name,Some(Coq.foundation))
      val nt = new NestedModule(OMMOD(parent.path),LocalName(uri.path.last),th) // TODO handle sections
      th.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? "Section")))
      log("Section " + th.path.toString + " ~> " + nt.path.toString)
      controller add nt
      statements.foreach(doDecl(th,_))
      // controller add PlainInclude(th.path,parent.path)
    case coqxml.VARIABLE(uri,as,components) =>
      val (name,tp,df) = components.collectFirst {
        case coqxml.Variable(nm,params,_,body,tptm) =>
          (LocalName(nm),Some(tptm.tm.toOMDoc(Map.empty,Map.empty)),body.map(_.tm.toOMDoc(Map.empty,Map.empty))) // TODO
      }.get
      // val tp : Option[Term] = None
      // val df : Option[Term] = None
      val c = Constant(parent.toTerm,name,Nil,tp,df,Some("Variable"))
      c.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
      log("Variable " + c.path.toString + ":: " + c.toString)
      controller add c
    case coqxml.constantlike(uri,as,components) =>
      if (components.isEmpty) {
        println(components)
        ???
      }
      val (name,tp,df) = components.collectFirst {
        case coqxml.ConstantType(namei,params,id,tpi) =>
          val dfi = components.collectFirst {
            case coqxml.ConstantBody(_,_,_,body) =>
              body.toOMDoc(Map.empty,Map.empty) // TODO
          }
          (LocalName(namei),Some(tpi.toOMDoc(Map.empty,Map.empty)),dfi) // TODO
      }.getOrElse {
        components.collectFirst {
          case coqxml.InductiveDefinition(noParams,_,_,tps) =>
            tps match {
              case coqxml.InductiveType(namei,indbool,_,arity,consts) :: Nil =>
                return () // TODO
              case _ =>
                ???
            }
        }.get
      }
      val c = Constant(parent.toTerm,name,Nil,tp,df,Some(as))
      c.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
      log("Definition " + c.path.toString + ":: " + c.toString)
      controller add c
    case _ =>
      println(elem.getClass)
      ???
  }

  class Parser(tfile: File) {
    def parse: List[coqxml.theorystructure] = {
      val topnode = {
        new XhtmlParser(scala.io.Source.fromString(File.read(tfile))).initialize.document()
      }//xml.readFile(f)
      topnode.flatMap(doStatement(_,List(tfile.name.split('.').head))).toList
    }

    private def doStatement(n : Node,namespaces : List[String] = Nil,sections : List[String] = Nil) : List[coqxml.theorystructure] = n match {
      case <html>{ch @ _*}</html> =>
        val ls = ch.find(_.label=="body").get.child
        val sts = ls.flatMap(doStatement(_,namespaces,sections))
        //ls.foreach(println)
        // TODO Documents
        sts.toList
      case <br/> => Nil
      case <hr/> => Nil
      case <b>Require</b> => Nil // TODO include?
      case <div/> | <div> </div> => Nil
      case <span>{ch @ _*}</span> => ch.flatMap(doStatement(_,namespaces,sections)).toList
      case a @ <a>{lnk}</a> if (a\"@href").mkString.startsWith("theory:/") =>
        val uri = URI((a\"@href").mkString)
        // TODO something
        Nil
      case txt : scala.xml.Text =>
        // TODO something
        Nil
      case at : scala.xml.Atom[_] =>
        // TODO something
        Nil
      case <a>{_*}</a> =>
        // TODO something
        Nil
      case <ul>{_*}</ul> =>
        // TODO something
        Nil
      case <h1>{_*}</h1> =>
        // TODO something
        Nil
      case <h2>{_*}</h2> =>
        // TODO something
        Nil
      case <h3>{_*}</h3> =>
        // TODO something
        Nil
      case <h4>{_*}</h4> =>
        // TODO something
        Nil
      case <pre>{_*}</pre> =>
        // TODO something
        Nil
      case <a></a> | <a/> => Nil
      case <i>{_*}</i> =>
        // TODO something
        Nil
      case <sup>{_*}</sup> =>
        // TODO something
        Nil
      case <SUP>{_*}</SUP> =>
        // TODO something
        Nil
      case <sub>{_*}</sub> =>
        // TODO something
        Nil
      case <SUB>{_*}</SUB> =>
        // TODO something
        Nil
      case <b>{_*}</b> =>
        // TODO something
        Nil
      case <em>{_*}</em> =>
        // TODO something
        Nil
      case <I>{_*}</I> =>
        // TODO something
        Nil
      case <style>{_*}</style> =>
        // TODO something
        Nil
      case _ : scala.xml.Comment => Nil
      case sec @ <SECTION>{ch @ _*}</SECTION> =>
        val uri = URI((sec\"@uri").mkString)
        // log("Section " + uri.toString)
        val recs = ch.flatMap(doStatement(_,namespaces,sections ::: List(uri.path.last))).toList
        List(coqxml.SECTION(uri,recs))
      case d @ <VARIABLE/> =>
        val uri = URI((d\"@uri").mkString)
        // log(uri.toString)
        val as = (d\"@as").mkString
        List(coqxml.VARIABLE(uri,as,handleXml(namespaces ::: sections,uri.path.last)))
      case d @ <DEFINITION/> =>
        val uri = URI((d\"@uri").mkString)
        // log(uri.toString)
        val as = (d\"@as").mkString
        List(coqxml.DEFINITION(uri,as,handleXml(namespaces,uri.path.last)))
      case d @ <THEOREM/> =>
        val uri = URI((d\"@uri").mkString)
        // log(uri.toString)
        val as = (d\"@as").mkString
        List(coqxml.THEOREM(uri,as,handleXml(namespaces,uri.path.last)))
      case d @ <AXIOM/> =>
        val uri = URI((d\"@uri").mkString)
        val as = (d\"@as").mkString
        List(coqxml.AXIOM(uri,as,handleXml(namespaces,uri.path.last)))
      case m @ <MODULE>{ls @ _*}</MODULE> =>
        val uri = URI((m\"@uri").mkString)
        val as = (m\"@as").mkString
        val components = ls.flatMap(doStatement(_,namespaces ::: List(uri.path.last),sections)).toList
        List(coqxml.MODULE(uri,as,components))
      case node =>
        val nd = node
        println(node.mkString)
        ???
    }

    private def handleXml(pathstr : List[String],name:String) = {
      val path = pathstr.foldLeft(tfile.up)((f,s) => f / s)
      val nf =  path / (name + ".xml")
      val nftp = path / (name + ".types.xml")
      val nfbd = path / (name + ".body.xml")
      List(nf,nftp,nfbd).flatMap {f =>
        if (f.exists()) {
          // log("Parsing XML: " + f)
          Some(handleNode(xml.readFile(f)))
        } else None
      }
    }

    private def handleNode(n : Node) : CoqEntry = n match {
      case it @ <InnerTypes>{ch @ _*}</InnerTypes> =>
        val uri = URI((it \ "@of").mkString)
        coqxml.InnerTypes(uri,ch.map(handleNode(_).asInstanceOf[coqxml.TYPE]).toList)
      case tp @ <TYPE><synthesized>{tm}</synthesized></TYPE> =>
        val of = (tp\"@of").mkString
        val term = handleNode(tm).asInstanceOf[coqxml.term]
        // term.toOMDoc(Map.empty,Map.empty) // debug
        coqxml.TYPE(of,term)
      case variable @ <Variable>{ch @ _*}</Variable> =>
        val name = (variable\"@name").mkString
        val params = (variable\"@params").mkString.split(' ').toList.filter(_ != "")
        val id = (variable\"@id").mkString
        val body = (variable\"body").headOption.map(handleNode).asInstanceOf[Option[coqxml.body]]
        val tp = handleNode((variable\"type").head).asInstanceOf[coqxml._type]
        body.foreach(_.tm.toOMDoc(Map.empty,Map.empty)) // debug
        tp.tm.toOMDoc(Map.empty,Map.empty) // debug
        coqxml.Variable(name,params,id,body,tp)
      case ctp @ <ConstantType>{tm}</ConstantType> =>
        val name = (ctp\"@name").mkString
        val params = (ctp\"@params").mkString.split(' ').toList
        val id = (ctp\"@id").mkString
        val tp = handleNode(tm).asInstanceOf[coqxml.term]
        tp.toOMDoc(Map.empty,Map.empty) // debug
        coqxml.ConstantType(name,params,id,tp)
      case ind @ <InductiveDefinition>{ch @ _*}</InductiveDefinition> =>
        val id = (ind\"@id").mkString
        val noParams = (ind\"@noParams").mkString.toInt
        val params = (ind\"@params").mkString.split(' ').toList
        val tps = (ind\"InductiveType").map(handleNode(_).asInstanceOf[coqxml.InductiveType]).toList
        coqxml.InductiveDefinition(noParams,params,id,tps)
      case cb @ <ConstantBody>{tm}</ConstantBody> =>
        val foruri = URI((cb\"@for").mkString)
        val params = (cb\"@params").mkString.split(' ').toList
        val id = (cb\"@id").mkString
        val body = handleNode(tm).asInstanceOf[coqxml.term]
        coqxml.ConstantBody(foruri,params,id,body)
      case ind @ <InductiveType>{ch @ _*}</InductiveType> =>
        val id = (ind\"@id").mkString
        val name = (ind\"@name").mkString
        val inductive = (ind\"@id").mkString match {
          case "true" => true
          case _ => false
        }
        val arity = handleNode((ind\"arity").head).asInstanceOf[coqxml.arity]
        val constructors = (ind\"Constructor").map(handleNode(_).asInstanceOf[coqxml.Constructor]).toList
        coqxml.InductiveType(name,inductive,id,arity,constructors)
      case prod @ <PROD>{ch @ _*}</PROD> =>
        val tp = (prod\"@type").mkString
        val decls = (prod\"decl").map(handleNode(_).asInstanceOf[coqxml.decl]).toList
        val target = handleNode((prod\"target").head)
        coqxml.PROD(tp,decls,target.asInstanceOf[coqxml.target])
      case <type>{tm}</type> =>
        coqxml._type(handleNode(tm).asInstanceOf[coqxml.term])
      case decl @ <decl>{ch}</decl> =>
        val id = (decl\"@id").mkString
        val _type = (decl\"@type").mkString
        val tm = handleNode(ch).asInstanceOf[coqxml.term]
        // log(id + ": " + _type + ": " + tm)
        coqxml.decl(id,_type,tm)
      case mutind @ <MUTIND/> =>
        val uri = URI((mutind\"@uri").mkString)
        val noType = (mutind\"@noType").mkString.toInt
        val id = (mutind\"@id").mkString
        coqxml.MUTIND(uri,noType,id)
      case <target>{tm}</target> =>
        coqxml.target(handleNode(tm).asInstanceOf[coqxml.term])
      case sort @ <SORT/> =>
        val value = (sort\"@value").mkString
        val id = (sort\"@id").mkString
        // log("Sort: " + coqxml.SORT(value,id).toOMDoc)
        coqxml.SORT(value,id)
      case <arity>{tm}</arity> =>
        coqxml.arity(handleNode(tm).asInstanceOf[coqxml.term])
      case cons @ <Constructor>{tm}</Constructor> =>
        val name = (cons\"@name").mkString
        coqxml.Constructor(name,handleNode(tm).asInstanceOf[coqxml.term])
      case appl @ <APPLY>{tms @ _*}</APPLY> =>
        val id = (appl\"@id").mkString
        val sort = (appl\"@sort").mkString
        val terms = tms.map(handleNode(_).asInstanceOf[coqxml.term]).toList
        coqxml.APPLY(id,sort,terms)
      case rel @ <REL/> =>
        val value = (rel\"@value").mkString.toInt
        val binder = (rel\"@binder").mkString
        val id = (rel\"@id").mkString
        val idref = (rel\"@idref").mkString
        val sort = (rel\"@sort").mkString
        // log("OMV: " + value + ": " + binder + ": " + idref + ": " + id)
        coqxml.REL(value,binder,id,idref,sort)
      case mut @ <MUTCONSTRUCT/> =>
        val uri = URI((mut\"@uri").mkString)
        val noType = (mut\"@noType").mkString.toInt
        val noConstr = (mut\"@noConstr").mkString.toInt
        val id = (mut\"@id").mkString
        val sort = (mut\"@sort").mkString
        coqxml.MUTCONSTRUCT(uri,noType,noConstr,id,sort)
      case v @ <VAR/> =>
        val uri = URI((v\"@uri").mkString)
        val id = (v\"@id").mkString
        val sort = (v\"@sort").mkString
        coqxml.VAR(uri,id,sort)
      case lam @ <LAMBDA>{ch @ _*}</LAMBDA> =>
        val sort = (lam\"@sort").mkString
        val decls = (lam\"decl").map(handleNode(_).asInstanceOf[coqxml.decl]).toList
        val target = handleNode((lam\"target").head).asInstanceOf[coqxml.target]
        // log("Lambda: " + sort + ": " + decls.mkString(", "))
        coqxml.LAMBDA(sort,decls,target)
      case fix @ <FIX>{ch @ _*}</FIX> =>
        val noFun = (fix\"@noFun").mkString.toInt
        val id = (fix\"@id").mkString
        val sort = (fix\"@sort").mkString
        val funs = ch.map(handleNode(_).asInstanceOf[coqxml.FixFunction]).toList
        coqxml.FIX(noFun,id,sort,funs)
      case fix @ <COFIX>{ch @ _*}</COFIX> =>
        val noFun = (fix\"@noFun").mkString.toInt
        val id = (fix\"@id").mkString
        val sort = (fix\"@sort").mkString
        val funs = ch.map(handleNode(_).asInstanceOf[coqxml.CofixFunction]).toList
        coqxml.COFIX(noFun,id,sort,funs)
      case fix @ <FixFunction>{ch @ _*}</FixFunction> =>
        val id = (fix\"@id").mkString
        val name = (fix\"@name").mkString
        val recIndex = (fix\"@recIndex").mkString.toInt
        val tp = handleNode((fix\"type").head).asInstanceOf[coqxml._type]
        val body = handleNode((fix\"body").head).asInstanceOf[coqxml.body]
        coqxml.FixFunction(name,id,recIndex,tp,body)
      case fix @ <CofixFunction>{ch @ _*}</CofixFunction> =>
        val id = (fix\"@id").mkString
        val name = (fix\"@name").mkString
        // val recIndex = (fix\"@recIndex").mkString.toInt
        val tp = handleNode((fix\"type").head).asInstanceOf[coqxml._type]
        val body = handleNode((fix\"body").head).asInstanceOf[coqxml.body]
        coqxml.CofixFunction(id,name,tp,body)
      case <body>{tm}</body> =>
        coqxml.body(handleNode(tm).asInstanceOf[coqxml.term])
      case mc @ <MUTCASE>{ch @ _*}</MUTCASE> =>
        val uriType = URI((mc\"@uriType").mkString)
        val noType = (mc\"@noType").mkString.toInt
        val id = (mc\"@id").mkString
        val sort = (mc\"@sort").mkString
        val ptp = handleNode((mc\"patternsType").head).asInstanceOf[coqxml.patternsType]
        val it = handleNode((mc\"inductiveTerm").head).asInstanceOf[coqxml.inductiveTerm]
        val patterns = (mc\"pattern").map(handleNode(_).asInstanceOf[coqxml.pattern]).toList
        coqxml.MUTCASE(uriType,noType,id,sort,ptp,it,patterns)
      case <patternsType>{tm}</patternsType> =>
        coqxml.patternsType(handleNode(tm).asInstanceOf[coqxml.term])
      case <inductiveTerm>{tm}</inductiveTerm> =>
        coqxml.inductiveTerm(handleNode(tm).asInstanceOf[coqxml.term])
      case <pattern>{tm}</pattern> =>
        coqxml.pattern(handleNode(tm).asInstanceOf[coqxml.term])
      case c @ <CONST/> =>
        val uri = URI((c\"@uri").mkString)
        val id = (c\"@id").mkString
        val sort = (c\"@sort").mkString
        coqxml.CONST(uri,id,sort)
      case cast @ <CAST>{ch @ _*}</CAST> =>
        val id = (cast\"@id").mkString
        val sort = (cast\"@sort").mkString
        val tm = handleNode((cast\"term").head).asInstanceOf[coqxml.term]
        val tp = handleNode((cast\"type").head).asInstanceOf[coqxml._type]
        coqxml.CAST(id,sort,tm,tp)
      case <term>{ch}</term> =>
        handleNode(ch)
      case let @ <LETIN>{ch @ _*}</LETIN> =>
        val sort = (let\"@sort").mkString
        val defs = (let\"def").map(handleNode(_).asInstanceOf[coqxml._def]).toList
        val target = handleNode((let\"target").head).asInstanceOf[coqxml.target]
        coqxml.LETIN(sort,defs,target)
      case df @ <def>{ch}</def> =>
        val id = (df\"@id").mkString
        val sort = (df\"@sort").mkString
        val binder = (df\"@binder").mkString
        coqxml._def(id,sort,binder,handleNode(ch).asInstanceOf[coqxml.term])
      case inst @ <instantiate>{ch @ _*}</instantiate> =>
        val id = (inst\"@id").mkString
        val oo = handleNode(ch.head).asInstanceOf[coqxml.objectOccurence]
        val args = (inst\"arg").map(handleNode(_).asInstanceOf[coqxml.arg])
        coqxml.instantiate(id,oo,args.toList)
      case arg @ <arg>{ch}</arg> =>
        val relUri = URI((arg\"@relUri").mkString)
        coqxml.arg(relUri,handleNode(ch).asInstanceOf[coqxml.term])
      case proj @ <PROJ>{ch}</PROJ> =>
        val id = (proj\"@id").mkString
        val sort = (proj\"@sort").mkString
        val noType = (proj\"@noType").mkString.toInt
        val uri = URI((proj\"@uri").mkString)
        val tm = handleNode(ch).asInstanceOf[coqxml.term]
        coqxml.PROJ(uri,noType,id,sort,tm)
      case _ =>
        println(n.mkString)
        ???
    }
  }
}