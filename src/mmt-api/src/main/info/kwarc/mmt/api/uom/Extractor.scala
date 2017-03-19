package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import frontend._
import modules._
import notations._
import libraries._
import documents._
import symbols._
import archives._
import utils._
import objects._


/** apply/unapply methods for the constructor Scala(code: String): Term to represent escaped Scala code in an MMT Term */
object Scala {
   val _path = DPath(utils.mmt.baseURI / "urtheories") ? "Scala"
   object Opaque {
      def apply(t: String) = OMSemiFormal(Text("scala", t))
      def unapply(t: Term) : Option[String] = t match {
         case OMSemiFormal(List(Text("scala", t))) => Some(t)
         case _ => None
      }
   }
   object Lambda {
      val path = _path ? "Lambda"
      def apply(con: Context, t: Term) = OMBIND(OMID(this.path), con, t) 
      def unapply(t: Term) : Option[(Context,Term)] = t match {
         case OMBIND(OMID(this.path), con, t) => Some((con,t))
         case t => Some((Context(),t))
      }
   }
   def symbol(s: String) = OMS(_path ? s)
}

object OpenMath {
   val _path = DPath(utils.URI("http", "www.openmath.org") / "cd") ? "OpenMath"
}

import GenericScalaExporter._

class OpenMathScalaExporter extends FoundedExporter(OpenMath._path, Scala._path) {
   val key = "om-scala"
   override val outExt = "scala"
   override protected val folderName = "NAMESPACE"
   
   private val imports = "import info.kwarc.mmt.api._\n" + "import objects._\n" + "import uom._\n" +
    "import ConstantScala._\n"

   private def arityToScala(arity: Arity) : List[(String,String)] = arity.components.map {
      case SimpArg(n,_) => ("x" + n.abs, "Term")
      case LabelArg(n,_,_) => ("x" + n.abs, "OML")
      case ImplicitArg(n,_) => ("x" + n.abs, "Term")
      case SimpSeqArg(n,_,_) => ("xs" + n.abs, "List[Term]")
      case LabelSeqArg(n,_,_,_) => ("xs" + n.abs, "List[OML]")
      case Var(n,_,None,_) => ("v" + n, "VarDecl")
      case Var(n,_,Some(_),_) => ("vs" + n, "Context")
   }
   private def lastArgIsSeq(arity: Arity) = ! arity.arguments.isEmpty && arity.arguments.last.isSequence
   private def lastVarIsSeq(arity: Arity) = ! arity.variables.isEmpty && arity.variables.last.isSequence
   
   private def termToScala(t: Term): String = t match {
      case OMA(f, args) =>
         val normalArgs = f match {
            case OMS(p) =>
               controller.globalLookup.getConstant(p).not.map {n =>
                  val a = n.arity
                  a.arguments.length - (if (lastArgIsSeq(a)) 1 else 0)
               }.getOrElse(0)
            case _ => 0
         }
         val argsRecurse = args.map(termToScala)
         var argsScala = argsRecurse.take(normalArgs).mkString(", ")
         if (normalArgs > 0 && normalArgs < argsRecurse.length)
             argsScala += ", "
         if (normalArgs < argsRecurse.length)
             argsScala += s"List(${argsRecurse.drop(normalArgs).mkString(", ")})"
         s"${termToScala(f)}($argsScala)"
      case OMBIND(b, con, sc) => s"${termToScala(b)}(${contextToScala(con)}, ${termToScala(sc)})"
      case OMS(p)   => nameToScalaQ(p)
      case OMV(n)   => n.toPath
      case l: OMLIT => s"OMLIT(${l.rt.toString})(${l.value.toString})"
      case t: OMSemiFormal => "(throw ParseError(\"\"\"informal term " + t.toString + "\"\"\"))"
   }
   // drops types, definiens
   private def contextToScala(c: Context): String = {
      val vars = c.variables.map("VarDecl(\"" + _.name + "\n,None,None)").mkString(", ") 
      s"Context($vars)"
   }
   
   private def applyMethods(arity: Arity) : String = {
     val scalaArgs = arityToScala(arity)
     // x1 :: ... :: xn :: Nil or x1 :: ... :: xsn
     var argListString   = scalaArgs.map(_._1).mkString(" :: ")
     if (! lastArgIsSeq(arity))
        argListString = argListString + ":: Nil"
     // (x1, ..., xn) or x1
     var argTupleString   = scalaArgs.map(_._1).mkString(", ")
     if (scalaArgs.length > 1)
        argTupleString = s"($argTupleString)"
     // (T1, ..., Tn) or T1
     var tpString    = scalaArgs.map(_._2).mkString(", ")
     if (scalaArgs.length > 1)
        tpString = s"($tpString)"
     // x1: T1, ..., xn: Tn
     val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
     // def apply(...) : Term = ...
     val app = if (arity.isApplication)
         s"    def apply($argtpString) = OMA(OMID(this.path), $argListString)\n"
     else if (arity.isPlainBinder)
         s"    def apply(vs1: Context, s2: Term) = OMBIND(OMID(this.path), vs1, s2)\n"
     else
         "  // no apply method generated for this arity\n"
     // def unapply(t: Term): Option[...] = ...
     val unapp = if (arity.isApplication)
         s"    def unapply(t: Term): Option[$tpString] = t match {\n" +
         s"      case OMA(OMID(this.path), $argListString) => Some($argTupleString)\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else if (arity.isPlainBinder)
         s"    def unapply(t: Term): Option[(Context, Term)] = t match {\n" +
         s"      case OMBIND(OMID(this.path), vs1, s2) => Some((vs1, s2))\n" +
         s"      case _ => None\n" +
         s"    }\n"
     else
         "  // no unapply methods generated for this arity\n"
     
     app + unapp
   }
   
   /** translates a theory T: OpenMath to a Scala trait */
   def exportCoveredTheory(t: DeclaredTheory) {
     val pack = dpathToScala(t.parent.doc)
     rh.writeln("package " + pack)
     rh.writeln(imports)
     // generating the trait
     val includes = t.getIncludesWithoutMeta.map(i => " with " + mpathToScala(i)).mkString("")
     rh.writeln(s"trait ${nameToScala(t.name)} extends RealizationInScala$includes {")
     t.getDeclarations foreach {
        case c: Constant =>
          if (c.rl == Some("FMP")) {
             val qname = nameToScalaQ(c.path)
             val qnameString = "\"" + qname + "\""
             rh.writeln(s"  val $qname = _assert($qnameString, _ => ${termToScala(c.tp.get)}, _ == logic1_true())\n")
          } else {
             val arity = c.not.map(_.arity).getOrElse(Arity.constant)
             val scalaArgs = arityToScala(arity)
             val argtpString = scalaArgs.map(p => p._1 + ": " + p._2).mkString(", ")
             var o = s"  def ${nameToScalaQ(c.path)}($argtpString): Term\n"
             rh.writeln(o)
          }
        case SimpleStructure(s, fromPath) if ! s.isInclude =>
             // unnamed structures have been handled above already
             rh.writeln("val " + nameToScalaQ(s.path) + ": " + mpathToScala(fromPath))
        case _ => 
     }
     rh.writeln("}\n")
     // generating the auxiliary object
     rh.writeln(s"object ${nameToScala(t.name)} extends TheoryScala {")
     val baseUri = t.parent.uri
     rh.writeln("  val _base = DPath(utils.URI(\"" + baseUri.scheme.getOrElse("") + 
        "\", \""+ baseUri.authority.getOrElse("") +"\")" + 
        baseUri.path.foldRight("")((a,b) => " / \""+ a + "\"" + b) +
        ")"
     )
     rh.writeln("  val _path = _base ? \"" + t.name + "\"")
     t.getDeclarations foreach {
	     case c: Constant =>
	         var o = ""
	         o +=  s"\n  object ${nameToScala(c.name)} extends ConstantScala {\n"
	         o +=  s"    val parent = _path\n"
	         o +=   "    val name = \"" + c.name + "\"\n"
	         c.not foreach {n =>
	            val a = n.arity
	            o += applyMethods(a)
	         }
	         o += "  }\n"
	         rh.writeln(o)
	      case _ => 
	   }
     rh.writeln("\n}\n")
   }
   
   /** currently skipped */
   def exportFunctor(v: DeclaredView) {}
   
   /** translates a view from (T: OpenMath) -> Scala to a Scala object implementing T */
   def exportRealization(v: DeclaredView) {
     val from = controller.globalLookup.getDeclaredTheory(v.from.toMPath) 
     val pack = dpathToScala(v.parent.doc)
     rh.writeln("package " + pack)
     rh.writeln(imports)
     // generating the object
     val trtPackage = dpathToScala(from.path.parent)
     rh.writeln("import " + trtPackage + "._\n")
     rh.writeln("import " + nameToScala(from.path.name) + "._\n")
     val includes = from.getIncludesWithoutMeta.flatMap {f =>
        v.getO(LocalName(ComplexStep(f))) match {
           case Some(PlainViewInclude(_,_,i)) => " with " + mpathToScala(i)
           case _ => ""
        }
     }.mkString("")
     rh.writeln(s"trait ${nameToScala(v.name)} extends ViewScala with ${nameToScala(from.path.name)}$includes {")
     var rules = ""
     from.getDeclarations foreach {
        case c: Constant =>
          if (c.rl != Some("FMP")) {
             val implemented = nameToScala(from.path.name) + "." + nameToScala(c.name) + ".path"
             val apath = v.path ? c.name
             val aO = v.getO(c.name)
             val arity = c.not.map(_.arity).getOrElse(Arity.constant)
             val scalaArgs = arityToScala(arity)
             val defaultNames = scalaArgs.map(_._1)
             val varTypes = scalaArgs.map(_._2)
             val scalaName = nameToScalaQ(c.path)
             val (varNames, impl) = aO match {
                case None =>
                   if (arity.isConstant)
                      (defaultNames, s"OMS($implemented)") //constants are often not implemented
                   else
                      (defaultNames, "throw Unimplemented(\"" + scalaName + "\")")
                case Some(a: Constant) => a.df match {
                   case Some(Scala.Lambda(con, Scala.Opaque(s))) =>
                      (con.variables.map(_.name.toPath), s)
                   case _ =>
                      (defaultNames, " //unexpected assignment in MMT view")
                }
                case Some(_) =>
                      (defaultNames, " //unexpected assignment in MMT view")
             }
             var o = ""
             o += s"  // UOM start " + apath.toPath + "\n"
             o += s"  def $scalaName(${varNames.zip(varTypes).map(p => p._1 + ": " + p._2).mkString(", ")}) : Term = {\n"
             o += s"    $impl\n"
             o += s"  }\n"
             o += s"  // UOM end " + apath.toPath + "\n"
             val normalArgs = arity.arguments.length - (if (lastArgIsSeq(arity)) 1 else 0)
             var implConstr = Range(0,normalArgs).map(_ => "A").mkString("") + (if (lastArgIsSeq(arity)) "S" else "")
             if (implConstr == "") implConstr = "constant"
             rules += s"  declares(Implementation.$implConstr($implemented)(${nameToScalaQ(c.path)} _))\n"
             rh.writeln(o)
          }
        case _ =>
     }
     rh.writeln(rules)
     rh.writeln("}\n")
     rh.writeln(s"object ${nameToScala(v.name)} extends ${nameToScala(v.name)}\n")
   }
   
   def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
      val pack = dpathToScala(dpath)
      rh.writeln("//Source file generated MMT\n")
      rh.writeln(s"package $pack")
      rh.writeln("import info.kwarc.mmt.api.uom._\n\n")
      rh.writeln(s"object $folderName extends DocumentScala {\n")
      modules foreach {bt =>
         controller.globalLookup.getModule(bt.contentMPath) match {
            case m: DeclaredTheory =>
               val p = s"$pack.${nameToScala(m.name)}"
               rh.writeln(s"  addTheory($p)")
            case m: DeclaredView =>
               val p = s"$pack.${nameToScala(m.name)}"
               rh.writeln(s"  addRealization($p)")
            case _ =>
         }
      }
      namespaces foreach {bt =>
         val p = dpathToScala(bt.contentDPath)
         rh.writeln(s"  addDocument($p.$folderName)")
      }
      rh.writeln("}")
   }
   def exportDocument(doc : Document, bt : BuildTask) {}

}


