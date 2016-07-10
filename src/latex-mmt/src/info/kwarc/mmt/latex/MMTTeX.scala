package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import web._
import utils._
import parser._
import symbols._
import modules._
import documents._
import frontend._
import objects._
import libraries._
import opaque._


import info.kwarc.mmt.stex._

object MMTTeX {
   def mmtCurrent(id: Int) = s"\\csname mmt@${id.toString}\\endcsname"
}

import MMTTeX._

class MMTTeX extends ShellExtension("mmttex") {
   def helpText: String = ""
   
   override def start(args : List[String]) {
     super.start(args)
     val li = new LatexInterpreter
     controller.extman.addExtension(li, Nil)
   }
   
   def run(shell: Shell, args: List[String]) = args match {
     case "sty" :: fileName :: Nil => genSty(fileName)
     case "gen" :: fileName :: Nil => genFiles(File(fileName).getAbsoluteFile)
     case "merge" :: fileName :: Nil => mergeFiles(File(fileName).getAbsoluteFile)
     case "make" :: fileName :: Nil => 
       val file = File(fileName).getAbsoluteFile
       genFiles(file)
       mergeFiles(file)
     case _ => throw LocalError("Invalid Arguments: " + args.mkString)
   }
   
   def genFiles(srcFile : File) : Boolean = {
     val base : DPath = DPath(FileURI(srcFile))
     _genFiles(srcFile, base)
     true
   }
   
   def _genFiles(srcFile : File, base : DPath) {
     val ext = srcFile.getExtension.getOrElse("tex") //default
     val altExt = if (ext == "tex") "mmt" else "tex"
     if (ext == "mmt") {
       genMMTFiles(srcFile, base)
     } else {//.tex case 
       //should call shell script for now do nothing
     }
     val altFile = srcFile.addExtension(altExt)
     if (altFile.exists) {//recursing
       _genFiles(altFile, base)
     }
   }
   
   def genMMTFiles(srcFile : File, base : DPath) {
     val ps = ParsingStream.fromFile(srcFile, formatOpt = Some("mmt"), dpathOpt = Some(base))
     val ec = new ErrorContainer(None)
     val doc = controller.read(ps, true)(ec)
     //write tex
     val lp = new LatexPresenter(new LatexObjectPresenter)
     initOther(lp)
     val outFile = srcFile.addExtension("tex") 
     val rh = new presentation.StringBuilder
     lp.apply(doc, standalone = true)(rh)
     if (lp.counter > 1) File.write(srcFile.addExtension("tex"), rh.get)
     //write omdoc
     val out = doc.toNodeResolved(controller.library, true).toString
     File.write(srcFile.addExtension("omdoc"), out)
   }
   
   def mergeFiles(srcFile : File) : Boolean = {
     val base : DPath = DPath(FileURI(srcFile))
     val cont = new Controller()
     _mergeFiles(srcFile, base, controller, cont)
     val newdoc = controller.getDocument(base).toNodeResolved(controller.library, true)
     File.write(srcFile.setExtension("omdoc"), newdoc.toString)
     true
   }
   
   var counter = 1

   def _mergeFiles(srcFile : File, base : DPath, cont : Controller, cont_extra : Controller) {
     val srcExt = srcFile.getExtension.getOrElse("tex")
     val omdocFile = srcFile.addExtension("omdoc")
     val mainDoc =  if (srcExt == "mmt") {
       val ps = ParsingStream.fromFile(omdocFile, dpathOpt = Some(base), formatOpt = Some("omdoc"))
       cont.read(ps, interpret = false)(ErrorThrower)
     } else {
       val importer = new STeXImporter()
       importer.init(cont)
       val src = File.read(omdocFile)
       importer.compileOne(src, base)
       cont.getDocument(base)
     }
     
     val altExt = if (srcExt == "tex") "mmt" else "tex"
     val altFile = srcFile.addExtension(altExt)
     if (altFile.exists) {
       _mergeFiles(altFile, base, cont_extra, cont)
       counter = 1
       traverse(mainDoc, base, cont, cont_extra)
     } //else nothing to do
   }
   
   def traverse(s : StructuralElement, from : DPath, cont : Controller, cont_other : Controller) : Unit = s match {
     case d : Document => 
       d.getDeclarations foreach {
          traverse(_, from, cont, cont_other)
       }
     case r : NRef => traverse(cont.get(r.target), from, cont, cont_other)
     case t : DeclaredTheory =>
       var refNames : List[LocalName] = Nil
       t.asDocument.getDeclarations foreach {
         case oe: UnknownOpaqueElement if (oe.format == "latex" || oe.format == "mmt") => 
           val refId = "mixref" + counter
           counter += 1
           val decls = cont_other.library.get(from / t.name / refId).getDeclarations flatMap {
             case s : SRef => List(cont_other.get(s.target))
             case _ => Nil
           }
           decls.reverse foreach {
             case c : Constant => 
               c.setDocumentHome(c.relativeDocumentHome.init)
               t.addAfterNarrative(c, oe.name)
             case _ => //ignore for now
           }
           refNames ::= oe.name
         case _ => //ignore for now
       }
       val tdoc = t.asDocument
       refNames foreach (tdoc.delete(_))
       t.getDeclarations foreach {
         case c : Constant => 
           val thy : DeclaredTheory = cont_other.get(t.path).asInstanceOf[DeclaredTheory]
           c.tp.map(t => c.tpC.update(TermContainer(tr(t, cont_other.library -> thy))))
           c.df.map(t => c.dfC.update(TermContainer(tr(t, cont_other.library -> thy))))
         case _ => //ignore
       }
       
     case _ => //nothing to do
   }
   
   object tr extends Traverser[(Library, DeclaredTheory)] {
     def traverse(t: Term)(implicit con : Context, p : (Library, DeclaredTheory)) : Term = t match {
       case OMV(name) if name.toPath.startsWith("mixref") => 
         val tm = p._2.get(name) match {
           case c : Constant => c.df.get
         }
         tm
       case OMV(name) => name.toString.split("@",-1).toList match {
         case tname :: sname :: sterm :: Nil => 
           try {
             val decl = if (tname == "") p._2.get(LocalName(sname)) 
                        else  p._1.get(p._2.parent ? tname ? sname)
             decl match {
               case c : Constant =>  
                 if (sterm == "") {
                   OMS(c.path)
                 } else {
                   val compS :: posS = sterm.split("\\.", -1).toList
                   val pos = Position(posS.map(_.toInt))
                     val comp = if (compS == "type") c.tp.get else c.df.get
                     comp.subobject(pos)._2.asInstanceOf[Term]
                 }
             }
           } catch {
             case e : Throwable => 
               val path = if (tname == "") p._2.path ? sname 
                          else  p._2.parent ? tname ? sname
               OMS(path)
           }
         case l => 
           Traverser(this, t)
       }
       case t => Traverser(this,t)
     }
   }
   
   
     
     /*
       case c : Constant if (c.name.toPath.startsWith("mixref")) => 
         val refId = c.name.toPath //.substring("mixref".length).toLowerCase()
         val decls = cont_other.library.get(from / t.name / refId).getDeclarations flatMap {
           case s : SRef => List(cont_other.get(s.target))
           case _ => Nil
         } 
         decls.reverse foreach {
           case fc : Constant =>
             val newC = new FinalConstant(OMMOD(t.path), fc.name, fc.alias, fc.tpC, fc.dfC, fc.rl, fc.notC)
             t.add(newC, Some(c.name))
           case _ => //ignore
         }
         t.delete(c.name)
       }
       cont.library.update(t)
   }
*/
   
   def genSty(fname : String) : Boolean = {
      var response = ""
      val f = File(fname)
      val ps = ParsingStream.fromFile(f, formatOpt = Some("mmt"))
      val ec = new ErrorContainer(None)
      val doc = controller.read(ps, true)(ec)
      val mods = doc.getModulesResolved(controller.globalLookup)
      val op = controller.extman.get(classOf[LatexObjectPresenter]).headOption.getOrElse {
         val op = new LatexObjectPresenter()
         controller.extman.addExtension(op) // make sure it's initialized
         op
      }
      def doModule(m: ContentElement) { 
         m.getDeclarations.foreach {
            case c: Constant =>
               val name = c.name.toPath
               if (name.startsWith("mmt@") && c.df.isDefined) {
                  val id = name.substring(4).toInt
                  val df = c.df.get
                  val dfL = op.asString(df, Some(c.path $ DefComponent))
                  response += s"\\expandafter\\def${mmtCurrent(id)}{$dfL}\n"
               }
            case nm: NestedModule =>
               doModule(nm.module)
            case _ =>
         }
      }
      mods foreach doModule
      File.write(f.addExtension("sty"), response)
      true
   }
}