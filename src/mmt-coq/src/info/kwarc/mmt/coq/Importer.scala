package info.kwarc.mmt.coq

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{BuildFailure, BuildResult, BuildSuccess, BuildTask, Dim, LogicalDependency, MissingDependency, RedirectableDimension}
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{AbstractTheory, Theory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMID, OML, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.{File, URI, xml}
import info.kwarc.mmt.coq.coqxml.{CoqEntry, CoqXml, SupXML, TranslationState}
import info.kwarc.mmt.lf.ApplySpine

import scala.collection.mutable
import scala.util.Try
import scala.xml.{Node}
import scala.xml.parsing.XhtmlParser


case class CoqDependency(uri: URI) extends Throwable

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
        sys.exit()
    }

    val ns = bf.inPath.init.tail.foldLeft(Path.parseD("cic://" + bf.inPath.head,NamespaceMap.empty))((b,s) => b / s)
    val ls = mutable.LinkedHashSet.empty[URI]
    val (used,doc) = try {
      doDocument(bf.narrationDPath,ns,bf.inFile.name.replace(".theory.xml",""),es,ls)
    }catch {
      case CoqDependency(u) =>
        return MissingDependency(LogicalDependency(DPath(u.^) ? u.path.last) :: Nil,ls.toList.map(u => LogicalDependency(DPath(u.^) ? u.path.last)),Nil)
        // return BuildFailure(Nil,Nil)
    }
    index(doc)

    BuildSuccess(Nil,used.map(u => LogicalDependency(DPath(u.^) ? u.path.last)))
  }

  def doDocument(nb : DPath, dp : DPath,file : String,elems : List[coqxml.theorystructure], used_uris:mutable.LinkedHashSet[URI]): (List[URI],Document) = {
    val d = new Document(nb)
    val top = Theory(dp,LocalName(file),Some(Coq.foundation))
    // log("Adding " + top.path)
    controller add d
    controller add top
    controller add MRef(d.path,top.path)
    elems.foreach(doDecl(top,_,used_uris))
    (used_uris.toList,d)
  }

  object CoqURI {

    private def toomdoc(uri: URI, in: AbstractTheory): ContentPath = {
      val fulluri = URI(uri.toString.replace("cic:/","cic://"))
      var path = fulluri.path
      if (path.last contains ".")
        path = path.init ::: List(path.last.split('.').head)
      var inpath = in.modulePath.parent.uri.path
      var modpath = in.modulePath.name.steps.map(_.toString)

      var currentD: DPath = Path.parseD("cic://" + fulluri.authority.get, NamespaceMap.empty)
      var currentM: MPath = null

      var is_in = fulluri.authority == in.modulePath.parent.uri.authority
      while (path.nonEmpty) {
        if (is_in && inpath.nonEmpty) {
          if (path.head == inpath.head || (path.head + "_impl") == inpath.head) {
            currentD = currentD / inpath.head
            path = path.tail
            inpath = inpath.tail
          } else is_in = false
        } else if (is_in && modpath.nonEmpty) {
          if (path.head == modpath.head || (path.head + "_impl") == modpath.head) {
            if (currentM == null) {
              currentM = currentD ? modpath.head
              path = path.tail
              modpath = modpath.tail
            } else {
              currentM = currentM.parent ? (currentM.name / modpath.head)
              path = path.tail
              modpath = modpath.tail
            }
          } else is_in = false
        } else if (currentM == null) controller.getO(currentD ? path.head) match {
          case Some(_) =>
            currentM = currentD ? path.head
            path = path.tail
          case None =>
            currentD = currentD / path.head
            path = path.tail
        } else {
          val np = currentM.parent ? (currentM.name / path.head)
          controller.getO(np) match {
            case Some(_) =>
              currentM = np
              path = path.tail
            case None =>
              return currentM ? path.mkString("/") //throw Dependency(uri) //.split('.').head // TODO is split('.') correct?
          }
        }
      }
      if (currentM != null) currentM else ???
      /*
    while (paths.nonEmpty) {
      if (is_in && uri.)
      controller.getO(currentD ? paths.head) match {
        case Some(_) =>
          currentM = currentD ? paths.head
          paths = paths.tail
          while (paths.nonEmpty) {
            controller.getO(currentM.parent ? (currentM.name / paths.head)) match {
              case Some(_) =>
                currentM = currentM.parent ? (currentM.name / paths.head)
                paths = paths.tail
              case None =>
                return currentM ? paths.mkString("/").split('.').head
            }
          }
          return currentM
        case None =>
          currentD = currentD / paths.head
          paths = paths.tail
      }
    }
    error
    */
    }

    def toMPath(uri: URI, in: AbstractTheory): MPath = Try(toomdoc(uri, in)).toOption match {
      case Some(mp: MPath) =>
        // ensure(mp,in)
        mp
      case Some(_) =>
        ???
      case _ =>
        throw CoqDependency(uri) // TODO
    }

    def toGlobalName(uri: URI,in: AbstractTheory): GlobalName = Try(toomdoc(uri.^,in)).toOption match {
      case Some(mp :MPath) =>
        // ensure(gn,in)
        mp ? uri.path.last.split('.').head
      case Some(_) =>
        ???
      case _ =>
        throw CoqDependency(uri.^) // TODO
    }

    private def ensure(p: ContentPath, in: AbstractTheory): Unit = {
      val mp = p.toMPath
      var current = in
      while (current match {
        case _ : Theory => false
        case d : DerivedDeclaration if List("Module","ModuleType") contains d.feature => false
        case _ => true
      }) {
        current = controller.get(current.parent).asInstanceOf[AbstractTheory]
      }

      val parent = current.modulePath
      controller.library.getImplicit(mp, parent) match {
        case Some(_) =>
        case None => try {
          controller.add(PlainInclude(mp, parent), AtBegin)
        }
        catch {
          case e: ExtensionError =>
          case e =>
            println(e.getClass)
            ???
        }
      }
    }
  }

  def doDecl(parent : AbstractTheory, elem : coqxml.theorystructure, used_uris:mutable.LinkedHashSet[URI]): Unit = {
    val state = TranslationState(controller,CoqURI.toMPath(_,parent),CoqURI.toGlobalName(_,parent),parent)
    elem match {
      // Simple Modules
      case m:coqxml.MODULE =>

        val (tp,impl) = if (m.attributes.isEmpty) (OMS(Coq.noConf),false)
        else m.attributes match {
          case List(SupXML(coqxml.supertypes(ms))) =>
            val mps = ms.map {
              case coqxml.MREF(uri) =>
                CoqURI.toMPath(uri,parent)
              case coqxml.WITH(coqxml.MREF(uri),_) =>
                CoqURI.toMPath(uri,parent)
                // TODO
            }
            (ApplySpine(OMS(Coq.confsubtp),mps.map(OMMOD(_)):_*),false)
          case ls =>
            val mps = ls.map {
              case coqxml.ExprXML(coqxml.MREF(uri)) =>
                CoqURI.toMPath(uri,parent)
              case coqxml.ExprXML(coqxml.WITH(coqxml.MREF(uri),_)) =>
                CoqURI.toMPath(uri,parent)
              // TODO
            }
            (ApplySpine(OMS(Coq.confcolon),mps.map(OMMOD(_)):_*),false)
        } // <: ~~> SupXML(supertypes(List()))
        // : ~~> ExprXML(MREF())
        print("")
        val (name,comp) = tp match {
          case _ : OMID | ApplySpine(OMS(Coq.confsubtp),_) =>
            (LocalName(m.uri.path.last), m.components)
          case _ =>
            (LocalName(m.uri.path.last + "_impl"),m.componentsImpl)
        } // parent.name / m.uri.path.last

        val nt = new DerivedDeclaration(parent.toTerm,name,m.as,TermContainer(Some(tp)),NotationContainer()) //new NestedModule(OMMOD(parent.path),LocalName(uri.path.last),th) // TODO handle sections
        used_uris += m.uri
        controller add nt
        log("Module " + nt.modulePath.toString)
        if (m.params.nonEmpty) {
          nt.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? "Functor")))
          m.params foreach {
            case (name,coqxml.ExprXML(coqxml.MREF(uri)) :: Nil) =>
              nt.metadata.add(new MetaDatum(Coq.foundation ? "Parameter",OML(LocalName(name))))
              Modules.covariant(uri,LocalName(name),nt,state)
            case (name,coqxml.ExprXML(coqxml.WITH(coqxml.MREF(uri),_)) :: Nil) =>
              nt.metadata.add(new MetaDatum(Coq.foundation ? "Parameter",OML(LocalName(name))))
              Modules.covariant(uri,LocalName(name),nt,state)
              // TODO
          }
        }
        comp.foreach(doDecl(nt,_,used_uris))

        try {
          controller.simplifier(nt.path)
        } catch {
          case _:GetError =>
          case GeneralError(_) => // TODO err obviously something blablabla variables not declared in context blabla
        }

      case coqxml.MODULEExpr(uri,params,as,components,children) =>
        val name = parent.modulePath.name / uri.path.last
        val th = Theory(parent.modulePath.parent,name,Some(Coq.foundation))
        val nt = new NestedModule(OMMOD(parent.modulePath), LocalName(uri.path.last), th)
        nt.metadata.add(new MetaDatum(Coq.decltype, OMS(Coq.foundation ? as)))
        log("Module " + th.path.toString)
        controller add nt

        if (params.nonEmpty) {
          nt.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? "Functor")))
          params foreach {
            case (name,coqxml.ExprXML(coqxml.MREF(uri)) :: Nil) =>
              nt.metadata.add(new MetaDatum(Coq.foundation ? "Parameter",OML(LocalName(name))))
              Modules.covariant(uri,LocalName(name),nt.module.asInstanceOf[Theory],state)
            case (name,coqxml.ExprXML(coqxml.WITH(coqxml.MREF(uri),_)) :: Nil) =>
              nt.metadata.add(new MetaDatum(Coq.foundation ? "Parameter",OML(LocalName(name))))
              Modules.covariant(uri,LocalName(name),nt.module.asInstanceOf[Theory],state)
            // TODO
          }
        }

        components.collectFirst {
          case coqxml.ExprXML(tx) =>
            // val t = tx.toOMDoc(controller)
            // TODO
            def getRec(it : coqxml.theoryexpr) : (URI,List[URI]) = it match {
              case coqxml.MREF(iuri) =>
                (iuri,Nil)
              case coqxml.FUNAPP(f,args) =>
                val (h,fi) = getRec(f)
                (h,fi ::: args.flatMap { a =>
                  val rec = getRec(a)
                  rec._1 :: rec._2
                })
              case coqxml.WITH(a,b) =>
                val ra = getRec(a)
                //val rb = getRec(b)
                //(ra._1,rb._1 :: ra._2 ::: rb._2)
                ra
              case coqxml.ABS(iuri,mod) =>
                val r = getRec(mod)
                (iuri,r._1 :: r._2)
              case _ =>
                ???
            }
            val (modtp,incls) = getRec(tx)
            (modtp :: incls).foreach { u =>
              val p = CoqURI.toMPath(u,th)
              controller add PlainInclude(p,th.path)
            }
            Nil
        }.getOrElse{
          print("wut?")
          ???
        }
        children.foreach(doDecl(th,_,used_uris))
        if (as =="AlgebraicModule") controller add PlainInclude(th.path,parent.modulePath)

      case coqxml.MODULE(uri,params,as,comp,implcomp,modcomps,modcompsimpl) =>
        val name = parent.name / uri.path.last
        val th = Theory(parent.modulePath.parent,name,Some(Coq.foundation))
        // log("Adding " + th.path)
        //controller add th
        params map { u =>
          // TODO
        }
        val nt = new NestedModule(OMMOD(parent.modulePath), LocalName(uri.path.last), th)
        nt.metadata.add(new MetaDatum(Coq.decltype, OMS(Coq.foundation ? as)))
        log("Module " + th.path.toString)
        controller add nt

        if (as=="Module" && implcomp.isEmpty) {
          comp.foreach(doDecl(th,_,used_uris))
          modcomps foreach {
            case coqxml.SupXML(coqxml.supertypes(ts)) =>
            // TODO
            case coqxml.ExprXML(ts) =>
            // TODO
            case _ =>
              ???
          }
          controller add PlainInclude(th.path,parent.modulePath)
        }
        else if (as=="Module") {
          implcomp.foreach(doDecl(th,_,used_uris))
          (modcomps ::: modcompsimpl) foreach {
            case coqxml.SupXML(coqxml.supertypes(ts)) =>
            // TODO
            case coqxml.ExprXML(ts) =>
            // TODO
            case _ =>
              ???
          }
          controller add PlainInclude(th.path,parent.modulePath)
        }
        else if (as=="ModuleType" && implcomp.isEmpty) {
          comp.foreach(doDecl(th,_,used_uris))
          // controller add PlainInclude(th.path,parent.path)
        } else {
          ???
        }
      // if (implcomp.nonEmpty) implcomp foreach (doDecl(th,_)) else comp.foreach(doDecl(th,_))
      // controller add PlainInclude(th.path,parent.path)
      case coqxml.SECTION(uri,statements) =>
        val name = parent.name / uri.path.last
        //val th = Theory(parent.parent,name,Some(Coq.foundation))
        val nt = new DerivedDeclaration(parent.toTerm,LocalName(uri.path.last),"Section",TermContainer(None),NotationContainer()) //new NestedModule(OMMOD(parent.path),LocalName(uri.path.last),th) // TODO handle sections
        nt.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? "Section")))
        log("Section " + nt.path.toString)
        controller add nt
        statements.foreach(doDecl(nt,_,used_uris))
        try {
          controller.simplifier(nt.path)
        } catch {
          case _:GetError =>
          case GeneralError(_) => // TODO err obviously something blablabla variables not declared in context blabla
        }
      // controller add PlainInclude(th.path,parent.path)
      case coqxml.VARIABLE_(uri,as,components) =>
        val (name,tp,df) = components.collectFirst {
          case coqxml.TopXML(coqxml.Variable(nm,params,_,body,tptm)) =>
            (LocalName(nm),Some(Coqtp(tptm.tm.toOMDoc(state))),body.map(_.tm.toOMDoc(state))) // TODO
        }.getOrElse {
          ???
        }
        // val tp : Option[Term] = None
        // val df : Option[Term] = None
        val c = Constant(parent.toTerm,name,Nil,tp,df,Some("Variable"))
        c.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
        // log("Variable " + c.path.toString + ":: " + c.toString)
        controller add c
      case coqxml.constantlike(uri,as,components) if as == "Inductive" || as == "CoInductive" =>
        val tps = components.collectFirst {
          case coqxml.TopXML(coqxml.InductiveDefinition(noParams, _, _, tpn)) =>
            tpn
        }.getOrElse {
          components.foreach(println)
          ???
        }
        val dname = LocalName(tps.map {
          case coqxml.InductiveType(namei,_,_,_,_) => namei
        }.mkString + "_DEF")
        val dd = new DerivedDeclaration(parent.toTerm,dname,as,TermContainer(None),NotationContainer()) //new NestedModule(OMMOD(parent.path),LocalName(uri.path.last),th) // TODO handle sections
        controller add dd

        tps foreach {
          case coqxml.InductiveType(namei,indbool,_,arity,consts) =>
            val name = LocalName(namei)
            val tp = Some(Coqtp(arity.tm.toOMDoc(state)))
            val df = None // TODO
          val c = Constant(dd.toTerm,name,Nil,tp,df,Some("Type"))
            controller add c
            consts.zipWithIndex foreach {
              case (coqxml.Constructor(cname,ctp),i) =>
                controller add Constant(dd.toTerm,LocalName(cname),LocalName(namei + "_C_" + (i+1)) :: Nil,Some(ctp.toOMDoc(state)),None,Some("Constructor"))

            }
        }
        try {
          controller.simplifier(parent.path)
        } catch {
          case _:GetError =>
        }
      /*
      val (name,tp,df) = components.collectFirst {
        case coqxml.TopXML(coqxml.InductiveDefinition(noParams,_,_,tps)) =>
          tps match {
            case coqxml.InductiveType(namei,indbool,_,arity,consts) :: Nil =>
              (LocalName(namei),Some(arity.tm.toOMDoc(controller,parent.modulePath)),None) // TODO
            case _ =>
              return ()
          }
      }.getOrElse {
        ???
      }
      val c = Constant(parent.toTerm,name,Nil,tp,df,Some(as))
      c.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
      log("Definition " + c.path.toString + ":: " + c.toString)
      controller add c
      */
      case coqxml.constantlike(uri,as,components) if as == "Scheme" =>

      case coqxml.constantlike(uri,as,components) if as == "Record" =>
      // TODO
      case coqxml.constantlike(uri,as,components) =>
        val (name,tp,df) = components.collectFirst {
          case coqxml.TopXML(coqxml.ConstantType(namei,params,id,tpi)) =>
            val dfi = components.collectFirst {
              case coqxml.BodyXML(coqxml.ConstantBody(_,_,_,body)) =>
                body.toOMDoc(state) // TODO
            }
            val ret = (LocalName(namei),Some(Coqtp(tpi.toOMDoc(state))),dfi) // TODO
            ret
        }.getOrElse {
          println("Oops")
          ???
        }
        val c = Constant(parent.toTerm,name,Nil,tp,df,Some(as))
        c.metadata.add(new MetaDatum(Coq.decltype,OMS(Coq.foundation ? as)))
        // log("Definition " + c.path.toString + ":: " + c.toString)
        controller add c
      case coqxml.Requirement(uri) =>
        val mp = CoqURI.toMPath(uri,parent)//(new coqxml.TranslationState(controller,parent.modulePath))
      val inc = PlainInclude(mp,parent.modulePath)
        controller add inc
      case _ =>
        println(elem.getClass)
        ???
    }
  }

  class Parser(tfile: File) {
    def parse: List[coqxml.theorystructure] = {


      val topnode = {
        new XhtmlParser(scala.io.Source.fromString(File.read(tfile))).initialize.document()
      }//xml.readFile(f)
      /*
      val src = Source.fromString("<html><body>" + File.read(tfile))
      val cp = parsing.ConstructingParser.fromSource(src, false)
      val topnode = try {
        cp.document()(0)
      } catch {case e: Exception =>
        throw XMLError("XML error while parsing " + tfile + ": " + e.getMessage).setCausedBy(e)
      } finally {
        src.close
      }
      */
      topnode.flatMap(doStatement(_,List(tfile.name.split('.').head))).toList
    }

    private def doStatement(n : Node,namespaces : List[String] = Nil,sections : List[String] = Nil) : List[coqxml.theorystructure] = n match {
      case <file>{ch @ _*}</file> =>
        ch.flatMap(doStatement(_,namespaces,sections)).toList
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
      case <img/> => Nil
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
      case <tt>{_*}</tt> =>
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
        List(coqxml.VARIABLE_(uri,as,handleXml(namespaces ::: sections,uri.path.last)))
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
        val params = (m\"@params").mkString match {
          case "" => Nil
          case s =>
            val ps = s.split(' ').map(URI(_)).toList// List(URI(s))
            ps.map{u =>
              val name = u.path.last
              val xm = handleXml(namespaces ::: List(uri.path.last),name)
              (name,xm)
            }
            // TODO
        }
        if (as=="AlgebraicModule" || as=="AlgebraicModuleType") {
          val folder = namespaces.foldLeft(tfile.up)((f,s) => f / s) / uri.path.last
          /*
          if (folder.children.exists(!_.name.endsWith(".xml"))) {
            print("nope: " + folder)
            ???
          }
          */
          val subs = folder.children.collect {
            case f if f.name.contains(".con.") => f.name.split('.').head + ".con"
          }.distinct
          val components = handleXml(namespaces, uri.path.last)
          val children = subs.map(s => coqxml.CHILD(uri / s,handleXml(namespaces ::: List(uri.path.last),s)))
          List(coqxml.MODULEExpr(uri,params,as,components,children))
        } else {
          val components = ls.flatMap(doStatement(_, namespaces ::: List(uri.path.last), sections)).toList
          val implcomponents = if ((namespaces.foldLeft(tfile.up)((f, s) => f / s) / (uri.path.last + ".impl")).exists) {
            ls.flatMap(doStatement(_, namespaces ::: List(uri.path.last + ".impl"), sections)).toList
          } else Nil
          val ncomps = handleXml(namespaces, uri.path.last)
          val nimplcomps = handleXml(namespaces, uri.path.last + ".impl")
          List(coqxml.MODULE(uri, params, as, components, implcomponents, ncomps, nimplcomps))
        }
      case r @ <REQUIRE/> =>
        val uri = URI((r\"@uri").mkString)
        List(coqxml.Requirement(uri))
      case <TYPESYSTEM/> => Nil // TODO
      case <UNIVERSE/> => Nil // TODO
      case <CONSTRAINT/> => Nil // TODO
      case node =>
        val nd = node
        println(node.mkString)
        ???
    }

    private def handleXml(pathstr : List[String],name:String) = {
      val path = pathstr.foldLeft(tfile.up)((f,s) => f / s)
      val files : List[(File,CoqEntry => CoqXml)] = List(
        (path / (name + ".xml"),coqxml.TopXML),
        (path / (name + ".types.xml"),coqxml.TypesXML),
        (path / (name + ".body.xml"),coqxml.BodyXML),
        // (path / (name + ".constraints.xml"),e => e.asInstanceOf[coqxml.Constraints]),
        (path / (name + ".expr.xml"),e => coqxml.ExprXML(e.asInstanceOf[coqxml.theoryexpr])),
        (path / (name + ".impl.expr.xml"),e => coqxml.ExprXML(e.asInstanceOf[coqxml.theoryexpr])),
        (path / (name + ".sub.xml"),e => coqxml.SupXML(e match {
          case s@coqxml.supertypes(_) => s
          case s:CoqEntry => coqxml.supertypes(List(s))
        }))
      )
      files.flatMap {case (f,e) =>
        if (f.exists()) {
          // log("Parsing XML: " + f)
          Some(e(handleNode(xml.readFile(f))))
        } else None
      }
      /*
      val fsup = path / (name + ".sub.xml")
      if (fsup.exists) {
        log("Parsing XML: " + fsup)
        val src = Source.fromString("<file>" + File.read(fsup) + "</file>")
        val cp = parsing.ConstructingParser.fromSource(src, false)
        val N = try {
          cp.document()(0)
        } catch {case e: Exception =>
          throw XMLError("XML error while parsing " + fsup + ": " + e.getMessage).setCausedBy(e)
        } finally {
          src.close
        }
        val ns = N match {
          case <file>{ch@_*}</file> =>
            ch.map(handleNode)
        }
        coqxml.SupXML(ns.toList) :: ret
      } else ret
      */
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
        // body.foreach(_.tm.toOMDoc(controller)) // debug
        // tp.tm.toOMDoc(controller) // debug
        coqxml.Variable(name,params,id,body,tp)
      case ctp @ <ConstantType>{tm}</ConstantType> =>
        val name = (ctp\"@name").mkString
        val params = (ctp\"@params").mkString.split(' ').toList
        val id = (ctp\"@id").mkString
        val tp = handleNode(tm).asInstanceOf[coqxml.term]
        // tp.toOMDoc(controller) // debug
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
        val inductive = (ind\"@inductive").mkString match {
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
        val binder = (decl\"@binder").mkString
        // log(id + ": " + _type + ": " + tm)
        coqxml.decl(id,_type,if (binder!="") binder else "_",tm)
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
        coqxml._def(id,sort,if (binder!="") binder else "_",handleNode(ch).asInstanceOf[coqxml.term])
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
      case mod @ <MODULE/> =>
        val uri = URI((mod\"@uri").mkString)
        coqxml.MREF(uri)
      case app @ <APP>{ch@_*}</APP> =>
        val chs = ch.map(handleNode).asInstanceOf[List[coqxml.theoryexpr]]
        coqxml.FUNAPP(chs.head,chs.tail)
      case wit @ <WITH>{ch@_*}</WITH> =>
        val chs = ch.map(handleNode).asInstanceOf[List[coqxml.theoryexpr]]
        assert(chs.length == 2)
        coqxml.WITH(chs.head,chs(1))
      case df@ <DEFINITION>{ch}</DEFINITION> =>
        val chs = handleNode(ch).asInstanceOf[coqxml.term]
        val uri = URI((df\"@relURI").mkString)
        coqxml.THDEF(uri,chs)
      case df@ <ABS>{ch}</ABS> =>
        val chs = handleNode(ch).asInstanceOf[coqxml.theoryexpr]
        val uri = URI((df\"@uri").mkString)
        coqxml.ABS(uri,chs)
      case st@ <supertypes>{ch@_*}</supertypes> =>
        coqxml.supertypes(ch.map(handleNode).toList)
      // case <constraints/> =>
       // coqxml.Constraints(Nil)
      case cs@ <CONSTRAINTS>{ch@_*}</CONSTRAINTS> =>
        coqxml.Constraints(ch.map(handleNode).toList)
      case _ =>
        println(n.mkString)
        ???
    }
  }
}